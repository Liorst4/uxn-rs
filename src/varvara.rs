// Copyright (C) 2023 Lior Stern.
//
// This file is part of uxn-rs.
// uxn-rs is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or any later version.
//
// uxn-rs is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// uxn-rs. If not, see <https://www.gnu.org/licenses/>.

extern crate sdl2;

mod uxn;
use uxn::Host;

use std::io::{Read, Write};

#[repr(u8)]
enum ConsoleType {
    NoQueue = 0,
    StdIn = 1,
    Argument = 2,
    ArgumentSeparator = 3,
    ArgumentEnd = 4,
}

impl Default for ConsoleType {
    fn default() -> Self {
        ConsoleType::NoQueue
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct System {
    halt: uxn::Short,
    expansion: uxn::Short,
    friend: uxn::Short,
    metadata: uxn::Short,
    red: uxn::Short,
    green: uxn::Short,
    blue: uxn::Short,
    debug: u8,
    state: u8,
}

enum ExpansionCommand {
    Copy {
        /// Byte count
        length: u16,

        /// Source page number
        src_page: u16,

        /// Offset in the source page
        src_addr: u16,

        /// Destination page number
        dst_page: u16,

        /// Offset in the destination page number
        dst_addr: u16,
    },
}

impl ExpansionCommand {
    fn read_from_uxn(vm: &uxn::Uxn, address: u16) -> Option<ExpansionCommand> {
        let id = vm.read8(address)?;
        if id == 1 {
            return Some(ExpansionCommand::Copy {
                length: vm.read16(address + 1)?,
                src_page: vm.read16(address + 3)?,
                src_addr: vm.read16(address + 5)?,
                dst_page: vm.read16(address + 7)?,
                dst_addr: vm.read16(address + 9)?,
            });
        }
        return None;
    }
    /// The amount of pages in virtual memory
    const RAM_PAGES: usize = 0x10;

    fn page_number_to_virtual_address(page_number: u8) -> u32 {
        (page_number as u32 % Self::RAM_PAGES as u32) * 0x10000
    }

    fn read8_from_virtual_address(vm: &uxn::Uxn, host: &Varvara, virtual_address: u32) -> u8 {
        if virtual_address <= u16::MAX as u32 {
            vm.read8(virtual_address as u16).unwrap()
        } else {
            *host
                .rom_expansion
                .get(virtual_address as usize - u16::MAX as usize)
                .unwrap()
        }
    }

    fn write8_to_virtual_address(
        vm: &mut uxn::Uxn,
        host: &mut Varvara,
        virtual_address: u32,
        value: u8,
    ) {
        if virtual_address <= u16::MAX as u32 {
            vm.write8(virtual_address as u16, value).unwrap();
        } else {
            *host
                .rom_expansion
                .get_mut(virtual_address as usize - u16::MAX as usize)
                .unwrap() = value;
        }
    }

    fn perform(vm: &mut uxn::Uxn, host: &mut Varvara, address: u16) {
        match Self::read_from_uxn(vm, address) {
            Some(ExpansionCommand::Copy {
                length,
                src_page,
                src_addr,
                dst_page,
                dst_addr,
            }) => {
                let source_virtual_address =
                    Self::page_number_to_virtual_address(src_page as u8) + src_addr as u32;
                let destination_virtual_address =
                    Self::page_number_to_virtual_address(dst_page as u8) + dst_addr as u32;

                for i in 0..length as u32 {
                    let byte =
                        Self::read8_from_virtual_address(vm, host, source_virtual_address + i);
                    Self::write8_to_virtual_address(
                        vm,
                        host,
                        destination_virtual_address + i,
                        byte,
                    );
                }
            }
            _ => {}
        }
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct Console {
    vector: uxn::Short,
    read: u8,
    _pad: u32,
    console_type: ConsoleType,
    write: u8,
    error: u8,
    __pad: [u8; 6],
}

enum Sprite<'a> {
    /// 1bit sprite
    /// https://wiki.xxiivv.com/site/icn_format.html
    ICN(&'a [u8; 8]),

    /// 2bit sprite
    /// https://wiki.xxiivv.com/site/chr_format.html
    CHR(&'a [u8; 16]),
}

impl<'a> Sprite<'a> {
    const WIDTH: u8 = 8;
    const HEIGHT: u8 = 8;

    fn from_bytes(bytes: &'a [u8]) -> Option<Sprite<'a>> {
        if bytes.len() == 8 {
            let sprite_ref = unsafe { std::mem::transmute(bytes.as_ptr()) };
            Some(Sprite::ICN(sprite_ref))
        } else if bytes.len() == 16 {
            let sprite_ref = unsafe { std::mem::transmute(bytes.as_ptr()) };
            Some(Sprite::CHR(sprite_ref))
        } else {
            None
        }
    }

    fn in_uxn(uxn: &'a uxn::Uxn, address: u16, mode: SpriteMode) -> Option<Sprite<'a>> {
        match mode {
            SpriteMode::OneBit => {
                let slice = uxn.slice(address, 8)?;
                Self::from_bytes(slice)
            }
            SpriteMode::TwoBit => {
                let slice = uxn.slice(address, 16)?;
                Self::from_bytes(slice)
            }
        }
    }

    fn byte_count(&self) -> usize {
        match self {
            Sprite::ICN(sprite) => std::mem::size_of_val(sprite),
            Sprite::CHR(sprite) => std::mem::size_of_val(sprite),
        }
    }

    fn pixel(&self, y: u8, x: u8) -> u8 {
        assert!(y < Self::HEIGHT && x < Self::WIDTH);
        match self {
            Sprite::ICN(sprite) => (sprite[y as usize] >> (7 - x)) & 0x1,
            Sprite::CHR(sprite) => {
                let ch1 = (sprite[y as usize] >> (7 - x)) & 0x1;
                let ch2 = ((sprite[(y + 8) as usize] >> (7 - x)) & 0x1) << 1;
                ch1 + ch2
            }
        }
    }
}

#[test]
fn test_sanity_chr() {
    // From https://wiki.xxiivv.com/site/chr_format.html
    const TILE: [u8; 16] = [
        0xf8, 0xf8, 0xf8, 0xf8, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3e, 0x3e, 0x3e, 0x3e, 0x3e,
        0x00,
    ];

    const EXPECTED_PIXELS: [[u8; 8]; 8] = [
        [1, 1, 1, 1, 1, 0, 0, 0],
        [1, 1, 1, 1, 1, 0, 0, 0],
        [1, 1, 3, 3, 3, 2, 2, 0],
        [1, 1, 3, 3, 3, 2, 2, 0],
        [1, 1, 3, 3, 3, 2, 2, 0],
        [0, 0, 2, 2, 2, 2, 2, 0],
        [0, 0, 2, 2, 2, 2, 2, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
    ];

    let sprite = Sprite::from_bytes(&TILE).unwrap();
    let mut pixels: [[u8; 8]; 8] = Default::default();

    for y in 0..Sprite::HEIGHT {
        for x in 0..Sprite::WIDTH {
            pixels[y as usize][x as usize] = sprite.pixel(y, x);
        }
    }

    assert_eq!(EXPECTED_PIXELS, pixels);
}

#[test]
fn test_sanity_icn() {
    // From https://wiki.xxiivv.com/site/icn_format.html
    const TILE: [u8; 8] = [0x00, 0x3c, 0x42, 0x7e, 0x40, 0x42, 0x3c, 0x00];
    const EXPECTED_PIXELS: [[u8; 8]; 8] = [
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 1, 1, 0, 0],
        [0, 1, 0, 0, 0, 0, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 0, 1, 0],
        [0, 0, 1, 1, 1, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
    ];
    let sprite = Sprite::from_bytes(&TILE).unwrap();
    let mut pixels: [[u8; 8]; 8] = Default::default();
    for y in 0..Sprite::HEIGHT {
        for x in 0..Sprite::WIDTH {
            pixels[y as usize][x as usize] = sprite.pixel(y, x);
        }
    }

    assert_eq!(EXPECTED_PIXELS, pixels);
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Layer {
    Background = 0,
    Forground = 1,
}

fn rgb_to_palette(red: u16, green: u16, blue: u16) -> [u32; 4] {
    fn short_to_be_nibbles(short: u16) -> [u8; 4] {
        let bytes = u16::to_be_bytes(short);

        fn high_nibble(byte: u8) -> u8 {
            (byte & 0xf0) >> 4
        }

        fn low_nibble(byte: u8) -> u8 {
            byte & 0x0f
        }

        return [
            high_nibble(bytes[0]),
            low_nibble(bytes[0]),
            high_nibble(bytes[1]),
            low_nibble(bytes[1]),
        ];
    }

    /// 0x1 -> 0x11, 0xf -> 0xff
    fn nibble_to_palindrome_byte(nibble: u8) -> u8 {
        nibble | (nibble << 4)
    }

    let red = short_to_be_nibbles(red);
    let green = short_to_be_nibbles(green);
    let blue = short_to_be_nibbles(blue);

    let mut palette: [u32; 4] = Default::default();
    for nibble_index in 0..palette.len() {
        palette[nibble_index] = u32::from_be_bytes(
            [
                0xf,
                red[nibble_index],
                green[nibble_index],
                blue[nibble_index],
            ]
            .map(nibble_to_palindrome_byte),
        );
    }

    return palette;
}

#[test]
fn test_rgb_to_palette() {
    for (expected, (r, g, b)) in [
        (
            // From https://wiki.xxiivv.com/site/theme.html
            [0xff000000, 0xffaa55cc, 0xff66ccaa, 0xffffffff],
            (0x0a6f, 0x05cf, 0x0caf),
        ),
        (
            // From piano.rom
            [0xff000000, 0xffffffff, 0xffeecc22, 0xff333333],
            (0x0fe3, 0x0fc3, 0x0f23),
        ),
        (
            // From screen.rom
            [0xffffffff, 0xff000000, 0xff77eecc, 0xffff0000],
            (0xf07f, 0xf0e0, 0xf0c0),
        ),
    ] {
        let result = rgb_to_palette(r, g, b);
        println!("expected: {:#08x?}", expected);
        println!(
            "result: {:#08x?} (r: {:#04x} g: {:#04x} b: {:#04x})",
            result, r, g, b
        );
        assert_eq!(result, expected);
    }
}

#[derive(Default)]
pub struct Frame {
    pub width: usize,
    pub height: usize,

    // The area that changed during the last render
    x1: usize,
    y1: usize,
    x2: usize,
    y2: usize,

    /// Four RGB888 pixels representing red, green, blue and alpha
    palette: [u32; 4],

    /// Each item is a `palette` index (2bit)
    foreground: Vec<u8>,

    /// Each item is a `palette` index (2bit)
    /// Color0 is treated as transparent
    background: Vec<u8>,

    /// RGB888 pixels
    pub pixels: Vec<u32>,
}

impl Frame {
    /// Update the rectangle that contains the pixels that were affected by the last draw.
    fn set_the_delta_area(&mut self, x1: usize, y1: usize, x2: usize, y2: usize) {
        self.x1 = std::cmp::min(x1, self.x1);
        self.y1 = std::cmp::min(y1, self.y1);
        self.x2 = std::cmp::max(x2, self.x2);
        self.y2 = std::cmp::max(y2, self.y2);
    }

    fn fill(&mut self, layer: Layer, x1: u16, y1: u16, x2: u16, y2: u16, color: u8) {
        let layer = match layer {
            Layer::Forground => &mut self.foreground,
            Layer::Background => &mut self.background,
        };

        for y in y1..std::cmp::min(y2, self.height as u16) {
            for x in x1..std::cmp::min(x2, self.width as u16) {
                layer[x as usize + (y as usize * self.width)] = color;
            }
        }
    }

    fn blit(
        &mut self,
        layer: Layer,
        sprite: Sprite,
        start_x: u16,
        start_y: u16,
        color: u8,
        flip_x: bool,
        flip_y: bool,
    ) {
        let width = self.width as u16;
        let height = self.height as u16;
        let opaque = (color % 5) != 0;
        let layer = match layer {
            Layer::Forground => &mut self.foreground,
            Layer::Background => &mut self.background,
        };

        for row in 0..Sprite::HEIGHT {
            for column in (0..Sprite::WIDTH).rev() {
                let dx = if flip_x {
                    Sprite::WIDTH - 1 - column
                } else {
                    column
                } as u16;

                let dy = if flip_y {
                    Sprite::HEIGHT - 1 - row
                } else {
                    row
                } as u16;

                let x = start_x.wrapping_add(dx);
                let y = start_y.wrapping_add(dy);

                if x >= width || y >= height {
                    continue;
                }

                let channel: u8 = sprite.pixel(row, column);
                if !opaque && (channel == 0) {
                    continue;
                }

                /// Copied from https://wiki.xxiivv.com/site/varvara.html#screen
                const BLENDING: [[u8; 16]; 4] = [
                    [0, 0, 0, 0, 1, 0, 1, 1, 2, 2, 0, 2, 3, 3, 3, 0],
                    [0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
                    [1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1],
                    [2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2],
                ];

                let x = x as usize;
                let y = y as usize;
                let width = width as usize;
                layer[x.wrapping_add(y.wrapping_mul(width))] =
                    BLENDING[channel as usize][color as usize];
            }
        }
    }

    pub fn set_palette(&mut self, red: u16, green: u16, blue: u16) {
        self.palette = rgb_to_palette(red, green, blue);
        self.set_the_delta_area(0, 0, self.width, self.height);
    }

    pub fn resize(&mut self, width: u16, height: u16) {
        const MIN_WIDTH: u16 = 0x8;
        const MAX_WIDTH: u16 = 0x400;
        const MIN_HEIGHT: u16 = MIN_WIDTH;
        const MAX_HEIGHT: u16 = MAX_WIDTH;

        if width < MIN_WIDTH || height < MIN_HEIGHT || width >= MAX_WIDTH || height >= MAX_HEIGHT {
            return;
        }

        self.width = width as usize;
        self.height = height as usize;
        let pixel_count = self.width * self.height;

        self.background.resize(pixel_count, 0);
        self.foreground.resize(pixel_count, 0);
        self.pixels
            .resize(pixel_count * std::mem::size_of::<u32>(), 0);

        for layer in [Layer::Forground, Layer::Background] {
            self.fill(layer, 0, 0, self.width as u16, self.height as u16, 0);
        }
    }

    /// Merge background, foreground and palette into a single image (`pixels`)
    pub fn update_pixels(&mut self) {
        let w = self.width;
        let h = self.height;
        let x1 = self.x1;
        let y1 = self.y1;
        let x2 = if self.x2 > w { w } else { self.x2 };
        let y2 = if self.y2 > h { h } else { self.y2 };

        for y in y1..y2 {
            for x in x1..x2 {
                let i = x + y * w;
                let palette_index_for_background = self.background[i];
                let palette_index_for_foreground = self.foreground[i];
                let palette_index_for_pixels = if palette_index_for_foreground == 0 {
                    palette_index_for_background
                } else {
                    palette_index_for_foreground
                };
                self.pixels[i] = self.palette[palette_index_for_pixels as usize];
            }
        }

        self.y1 = 0xffff;
        self.x1 = self.y1;
        self.y2 = 0;
        self.x2 = self.y2;
    }
}

#[repr(u8)]
enum PixelMode {
    Pixel = 0,
    Fill = 1,
}

#[repr(u8)]
enum SpriteMode {
    OneBit = 0,
    TwoBit = 1,
}

trait Control {
    fn get(&self) -> u8;

    fn layer(&self) -> Layer {
        const LAYER_MASK: u8 = 0b01000000;
        if self.get() & LAYER_MASK != 0 {
            Layer::Forground
        } else {
            Layer::Background
        }
    }

    fn flip_y(&self) -> bool {
        const FLIP_Y_MASK: u8 = 0b00100000;
        self.get() & FLIP_Y_MASK != 0
    }

    fn flip_x(&self) -> bool {
        const FLIP_X_MASK: u8 = 0b00010000;
        self.get() & FLIP_X_MASK != 0
    }
}

#[derive(Default)]
#[repr(packed(1))]
pub struct PixelFlags {
    value: u8,
}

impl Control for PixelFlags {
    fn get(&self) -> u8 {
        self.value
    }
}

impl PixelFlags {
    fn color(&self) -> u8 {
        const COLOR_MASK: u8 = 0b00000011;
        self.value & COLOR_MASK
    }

    fn mode(&self) -> PixelMode {
        const PIXEL_MODE_MASK: u8 = 0b10000000;
        if (self.value & PIXEL_MODE_MASK) != 0 {
            return PixelMode::Fill;
        }
        return PixelMode::Pixel;
    }
}

#[derive(Default)]
#[repr(packed(1))]
pub struct SpriteFlags {
    value: u8,
}

impl Control for SpriteFlags {
    fn get(&self) -> u8 {
        self.value
    }
}

impl SpriteFlags {
    fn blending(&self) -> u8 {
        const BLENDING_MASK: u8 = 0b00001111;
        self.value & BLENDING_MASK
    }

    fn mode(&self) -> SpriteMode {
        const SPRITE_MODE_MASK: u8 = 0b10000000;
        if (self.value & SPRITE_MODE_MASK) != 0 {
            return SpriteMode::TwoBit;
        }
        return SpriteMode::OneBit;
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct AutoFlags {
    value: u8,
}

impl AutoFlags {
    fn length(&self) -> u8 {
        const LENGTH_SHIFT: u8 = 4;
        self.value >> LENGTH_SHIFT
    }

    fn addr(&self) -> bool {
        const ADDR_MASK: u8 = 0b00000100;
        (self.value & ADDR_MASK) != 0
    }

    fn y(&self) -> bool {
        const Y_MASK: u8 = 0b00000010;
        (self.value & Y_MASK) != 0
    }

    fn x(&self) -> bool {
        const X_MASK: u8 = 0b00000001;
        (self.value & X_MASK) != 0
    }
}

#[derive(Default)]
#[repr(packed(1))]
pub struct Screen {
    pub vector: uxn::Short,
    pub width: uxn::Short,
    pub height: uxn::Short,
    auto: AutoFlags,
    _pad: u8,
    x: uxn::Short,
    y: uxn::Short,
    addr: uxn::Short,
    pub pixel: PixelFlags,
    pub sprite: SpriteFlags,
}

impl Screen {
    pub fn draw_pixel(&mut self, frame: &mut Frame) {
        let ctrl = &self.pixel;
        let color = ctrl.color();
        let mut x = self.x.get();
        let mut y = self.y.get();
        let layer = ctrl.layer();
        match ctrl.mode() {
            PixelMode::Fill => {
                let mut x2 = frame.width as u16;
                let mut y2 = frame.height as u16;

                if ctrl.flip_x() {
                    x2 = x;
                    x = 0;
                }

                if ctrl.flip_y() {
                    y2 = y;
                    y = 0;
                }

                frame.fill(layer, x, y, x2, y2, color);
                frame.set_the_delta_area(x as usize, y as usize, x2 as usize, y2 as usize);
            }
            PixelMode::Pixel => {
                let width = frame.width as u16;
                let height = frame.height as u16;
                let layer = match layer {
                    Layer::Background => &mut frame.background,
                    Layer::Forground => &mut frame.foreground,
                };

                if x < width && y < height {
                    layer[x as usize + y as usize * width as usize] = color;
                }

                // Apply auto flags
                if self.auto.x() {
                    self.x.set(x.wrapping_add(1));
                }

                if self.auto.y() {
                    self.y.set(y.wrapping_add(1));
                }
            }
        }
    }

    pub fn draw_sprite(&mut self, uxn: &mut uxn::Uxn, frame: &mut Frame) {
        let ctrl = &self.sprite;
        let move_ = &self.auto;
        let length = move_.length();
        let x = self.x.get();
        let y = self.y.get();
        let mut addr = self.addr.get();
        let dx = if move_.x() { Sprite::WIDTH as u16 } else { 0 };
        let dy = if move_.y() { Sprite::HEIGHT as u16 } else { 0 };
        let layer = ctrl.layer();

        for i in 0..(length + 1) {
            let sprite = Sprite::in_uxn(uxn, addr, ctrl.mode()).unwrap();
            let byte_count = sprite.byte_count();

            frame.blit(
                layer,
                sprite,
                x + (dy * i as u16),
                y + (dx * i as u16),
                ctrl.blending(),
                ctrl.flip_x(),
                ctrl.flip_y(),
            );

            if move_.addr() {
                addr += byte_count as u16;
            }
        }

        frame.set_the_delta_area(
            x as usize,
            y as usize,
            x as usize + dy as usize * length as usize + Sprite::HEIGHT as usize,
            y as usize + dx as usize * length as usize + Sprite::WIDTH as usize,
        );

        if move_.x() {
            self.x.set(x.wrapping_add(dx as u16));
        }

        if move_.y() {
            self.y.set(y.wrapping_add(dy as u16));
        }

        if move_.addr() {
            self.addr.set(addr);
        }
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct ADSR {
    data: uxn::Short,
}

impl ADSR {
    fn attack(&self) -> u8 {
        const ATTACK_MASK: u16 = 0b1111000000000000;
        const ATTACK_SHIFT: u16 = 12;
        return ((self.data.get() & ATTACK_MASK) >> ATTACK_SHIFT) as u8;
    }

    fn decay(&self) -> u8 {
        const DECAY_MASK: u16 = 0b0000111100000000;
        const DECAY_SHIFT: u16 = 8;
        return ((self.data.get() & DECAY_MASK) >> DECAY_SHIFT) as u8;
    }

    fn sustain(&self) -> u8 {
        const SUSTAIN_MASK: u16 = 0b0000000011110000;
        const SUSTAIN_SHIFT: u16 = 4;
        return ((self.data.get() & SUSTAIN_MASK) >> SUSTAIN_SHIFT) as u8;
    }

    fn release(&self) -> u8 {
        const RELEASE_MASK: u16 = 0b0000000000001111;
        return (self.data.get() & RELEASE_MASK) as u8;
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct Pitch {
    data: u8,
}

impl Pitch {
    fn loop_(&self) -> bool {
        const LOOP_MASK: u8 = 0b10000000;
        (self.data & LOOP_MASK) != 0
    }

    fn note(&self) -> u8 {
        const NOTE_MASK: u8 = 0b01111111;
        const NOTE_SHIFT: u8 = 1;
        (self.data & NOTE_MASK) >> NOTE_SHIFT
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct Volume {
    data: u8,
}

impl Volume {
    fn left(&self) -> u8 {
        const LEFT_CHANNEL_MASK: u8 = 0b11110000;
        const LEFT_CHANNEL_SHIFT: u8 = 4;
        (self.data & LEFT_CHANNEL_MASK) >> LEFT_CHANNEL_SHIFT
    }

    fn right(&self) -> u8 {
        const RIGHT_CHANNEL_MASK: u8 = 0b00001111;
        self.data & RIGHT_CHANNEL_MASK
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct Audio {
    vector: uxn::Short,
    position: uxn::Short,
    output: u8,
    _pad: [u8; 3],
    adsr: ADSR,
    length: uxn::Short,
    addr: uxn::Short,
    volume: Volume,
    pitch: Pitch,
}

#[derive(Default)]
#[repr(packed(1))]
struct Controller {
    vector: uxn::Short,
    button: u8,
    key: u8,
    _pad: u8,
    _p2: u8,
    _p3: u8,
    _p4: u8,
    __pad: [u8; 8],
}

impl Controller {
    const A: u8 = 0b00000001;
    const B: u8 = 0b00000010;
    const SELECT: u8 = 0b00000100;
    const START: u8 = 0b00001000;
    const UP: u8 = 0b00010000;
    const DOWN: u8 = 0b00100000;
    const LEFT: u8 = 0b01000000;
    const RIGHT: u8 = 0b10000000;
}

#[derive(Default)]
#[repr(packed(1))]
struct Mouse {
    vector: uxn::Short,
    x: uxn::Short,
    y: uxn::Short,
    state: u8,
    _pad: [u8; 3],
    scrollx: uxn::Short,
    scrolly: uxn::Short,
    __pad: [u8; 2],
}

impl Mouse {
    const MOUSE1: u8 = 0b00000001;
    const MOUSE2: u8 = 0b00000010;
    const MOUSE3: u8 = 0b00000100;
}

#[derive(Default)]
#[repr(packed(1))]
struct File {
    _vector: uxn::Short,
    success: uxn::Short,
    stat: uxn::Short,
    delete: u8,
    append: u8,
    name: uxn::Short,
    length: uxn::Short,
    read: uxn::Short,
    write: uxn::Short,
}

fn complies_with_sandbox_rules(path: &std::path::Path) -> bool {
    if path.is_absolute() {
        eprintln!("ABSOLUTE PATHS AREN'T ALLOWED");
        return false;
    }

    if path.is_symlink() {
        eprintln!("SYMLINKS AREN'T ALLOWED");
        return false;
    }

    for i in path {
        if i == ".." {
            eprintln!("RELATIVE PATHS WITH .. AREN'T ALLOWED");
            return false;
        }
    }

    return true;
}

impl File {
    fn path<'a>(&self, uxn: &'a uxn::Uxn) -> Option<&'a std::path::Path> {
        let address_of_name_in_uxn = self.name.get();
        let mut name_byte_count: u16 = 0;
        while uxn.read8(address_of_name_in_uxn + name_byte_count)? != 0 {
            name_byte_count += 1;
        }
        let name = uxn.slice(address_of_name_in_uxn, name_byte_count)?;
        let name = std::str::from_utf8(name).ok()?;
        let path = std::path::Path::new(name);
        if !complies_with_sandbox_rules(path) {
            eprintln!("{} violated sandbox rules", name);
            return None;
        }
        return Some(path);
    }

    fn get_operation_length(&self) -> u16 {
        self.length.get()
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct DateTime {
    year: uxn::Short,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    dotw: u8,
    doty: uxn::Short,
    isdst: u8,
    _pad: [u8; 5],
}

impl DateTime {
    fn update(&mut self) {
        let mut calendar_time = libc::tm {
            tm_sec: 0,
            tm_min: 0,
            tm_hour: 0,
            tm_mday: 0,
            tm_mon: 0,
            tm_year: 0,
            tm_wday: 0,
            tm_yday: 0,
            tm_isdst: 0,
            tm_gmtoff: 0,
            tm_zone: std::ptr::null(),
        };
        let mut now: libc::time_t = 0;
        unsafe {
            libc::time(&mut now);
            libc::localtime_r(&now, &mut calendar_time);
        }

        self.year.set((calendar_time.tm_year + 1900) as u16);
        self.month = calendar_time.tm_mon as u8;
        self.day = calendar_time.tm_mday as u8;
        self.hour = calendar_time.tm_hour as u8;
        self.minute = calendar_time.tm_min as u8;
        self.second = calendar_time.tm_sec as u8;
        self.dotw = calendar_time.tm_wday as u8;
        self.doty.set(calendar_time.tm_yday as u16);
        self.isdst = calendar_time.tm_isdst as u8;
    }
}

#[repr(packed(1))]
#[derive(Default)]
struct DeviceIOMemory {
    system: System,
    console: Console,
    screen: Screen,
    audio: [Audio; 4],
    _pad: [u8; 0x10],
    controller: Controller,
    mouse: Mouse,
    file: [File; 2],
    datetime: DateTime,
    _reserved_pad: [[u8; 0x10]; 2],
    __pad: [u8; 0x10],
}

impl<'a> DeviceIOMemory {
    fn as_raw_bytes(&'a self) -> &'a [u8; uxn::IO_BYTE_COUNT] {
        unsafe { std::mem::transmute(self) }
    }

    fn read8(&self, offset: u8) -> Option<u8> {
        self.as_raw_bytes().get(offset as usize).map(|x| *x)
    }

    fn read16(&self, offset: u8) -> Option<u16> {
        let first = self.read8(offset)?;
        let second = self.read8(offset + 1)?;
        return Some(uxn::Short::u16_from_bytes([first, second]));
    }

    fn as_raw_bytes_mut(&'a mut self) -> &'a mut [u8; uxn::IO_BYTE_COUNT] {
        unsafe { std::mem::transmute(self) }
    }

    fn write8(&mut self, offset: u8, value: u8) -> Option<()> {
        *self.as_raw_bytes_mut().get_mut(offset as usize)? = value;
        Some(())
    }

    fn write16(&mut self, offset: u8, value: u16) -> Option<()> {
        let value = uxn::Short::u16_to_bytes(value);

        let first = self.as_raw_bytes_mut().get_mut((offset + 1) as usize)?;
        *first = value[1];

        let second = self.as_raw_bytes_mut().get_mut(offset as usize)?;
        *second = value[0];

        Some(())
    }
}

fn read_dir_entry(path: &std::path::Path) -> Option<Vec<u8>> {
    let metadata = path.metadata().ok()?;
    let size_str: String;
    if metadata.is_file() {
        let size = metadata.len();
        if size <= u16::MAX as u64 {
            size_str = format!("{:04x}", size);
        } else {
            size_str = "????".to_string();
        }
    } else {
        size_str = "----".to_string();
    }

    return Some(format!("{} {}", size_str, path.to_str()?).into_bytes());
}

fn read_dir(path: &std::path::Path) -> Option<Vec<Vec<u8>>> {
    let mut entries = vec![];
    for entry in std::fs::read_dir(path).ok()? {
        entries.push(read_dir_entry(&entry.ok()?.path())?);
    }
    return Some(entries);
}

enum OpenedPath {
    None,
    File {
        path: std::path::PathBuf,
        handle: std::fs::File,
    },
    Directory {
        path: std::path::PathBuf,
        entries: Vec<Vec<u8>>,
        read_index: usize,
    },
}

impl Default for OpenedPath {
    fn default() -> Self {
        OpenedPath::None
    }
}

#[derive(Default)]
struct ProgramMetadata {
    _varvara_version: u8,
    text: Vec<String>,
    // TODO: Extended (https://wiki.xxiivv.com/site/metadata.html)
}

impl ProgramMetadata {
    fn read_from_uxn(vm: &uxn::Uxn, address: u16) -> Option<ProgramMetadata> {
        let mut text = vec![];

        let varvara_version = vm.read8(address)?;

        let mut offset = 1;
        while offset < 256 {
            let b = vm.read8(address + offset)?;
            if b == 0 {
                break;
            }

            if text.is_empty() {
                text.push(String::new());
            }

            if b == 0x0a {
                text.push(String::new());
            } else {
                text.last_mut().unwrap().push(b as char);
            }

            offset += 1;
        }

        Some(ProgramMetadata {
            _varvara_version: varvara_version,
            text,
        })
    }
}

#[derive(Default)]
struct Varvara {
    io_memory: DeviceIOMemory,
    open_files: [OpenedPath; 2],
    frame: Frame,

    /// Parts of the ROM that don't fit inside the address space
    rom_expansion: Vec<u8>,

    /// Contains parsed metadata if the program wrote to .System/metadata during the previous cycle
    metadata_update: Option<ProgramMetadata>,
}

// TODO: Replace with `std::mem::offset_of` when it becomes stable.
macro_rules! offset_of_device_port {
    ($device:ident, $port: ident) => {{
        let base: *const DeviceIOMemory = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).$device.$port) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;
        offset
    }};
    ($device:ident, $device_index:expr, $port: ident) => {{
        let base: *const DeviceIOMemory = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).$device[$device_index].$port) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;
        offset
    }};
}

fn overlaps_device_port(target: u8, short_mode: bool, port_offset: u8) -> bool {
    if short_mode {
        (target == port_offset) || (target > 1 && ((target - 1) == port_offset))
    } else {
        target == port_offset
    }
}

macro_rules! targeted_device_port {
    ($target:expr, $short_mode:expr, $device:ident, $port: ident) => {{
        let offset = offset_of_device_port!($device, $port);
        overlaps_device_port($target, $short_mode, offset)
    }};
    ($target:expr, $short_mode:expr, $device:ident, $device_index:expr, $port: ident) => {{
        let offset = offset_of_device_port!($device, $device_index, $port);
        overlaps_device_port($target, $short_mode, offset)
    }};
}

impl uxn::Host for Varvara {
    fn dei(&mut self, _cpu: &mut uxn::Uxn, target: u8, short_mode: bool) -> Option<u16> {
        macro_rules! about_to_read_from {
            ($device:ident, $port:ident) => {
                targeted_device_port!(target, short_mode, $device, $port)
            };
            ($device:ident, $index: expr, $port:ident) => {
                targeted_device_port!(target, short_mode, $device, $index, $port)
            };
        }

        if about_to_read_from!(datetime, year)
            || about_to_read_from!(datetime, month)
            || about_to_read_from!(datetime, day)
            || about_to_read_from!(datetime, hour)
            || about_to_read_from!(datetime, minute)
            || about_to_read_from!(datetime, second)
            || about_to_read_from!(datetime, dotw)
            || about_to_read_from!(datetime, doty)
            || about_to_read_from!(datetime, isdst)
        {
            self.io_memory.datetime.update();
        }

        if about_to_read_from!(screen, width) || about_to_read_from!(screen, height) {
            self.io_memory.screen.height.set(self.frame.height as u16);
            self.io_memory.screen.width.set(self.frame.width as u16);
        }

        for i in 0..self.io_memory.audio.len() {
            if about_to_read_from!(audio, i, output) {
                // TODO: Update volume
            }

            if about_to_read_from!(audio, i, position) {
                // TODO: Update position
            }
        }

        if short_mode {
            self.io_memory.read16(target)
        } else {
            self.io_memory.read8(target).map(|x| x as u16)
        }
    }

    fn deo(&mut self, cpu: &mut uxn::Uxn, target: u8, value: u16, short_mode: bool) -> Option<()> {
        if short_mode {
            self.io_memory.write16(target, value)?;
        } else {
            self.io_memory.write8(target, value as u8)?;
        }

        macro_rules! written_to {
            ($device:ident, $port:ident) => {
                targeted_device_port!(target, short_mode, $device, $port)
            };
            ($device:ident, $index: expr, $port:ident) => {
                targeted_device_port!(target, short_mode, $device, $index, $port)
            };
        }

        if written_to!(console, write) {
            let bytes = [self.io_memory.console.write];
            std::io::stdout().write(&bytes).unwrap();
            std::io::stdout().flush().unwrap();
        }

        if written_to!(console, error) {
            let bytes = [self.io_memory.console.error];
            std::io::stderr().write(&bytes).unwrap();
            std::io::stderr().flush().unwrap();
        }

        if written_to!(system, debug) && self.io_memory.system.debug != 0 {
            eprintln!("Working stack: {}", cpu.working_stack);
            eprintln!("Return stack: {}", cpu.return_stack);
            std::io::stderr().flush().unwrap();
        }

        for i in 0..self.io_memory.file.len() {
            if written_to!(file, i, name) {
                || -> Option<()> {
                    let path = self.io_memory.file[i].path(cpu)?;
                    if !path.exists() {
                        let file = std::fs::File::create(path).ok()?;
                        self.open_files[i] = OpenedPath::File {
                            path: path.to_owned(),
                            handle: file,
                        };
                    } else if path.is_file() {
                        let file = std::fs::OpenOptions::new()
                            .read(true)
                            .write(true)
                            .append(self.io_memory.file[i].append != 0)
                            .open(path)
                            .ok()?;

                        self.open_files[i] = OpenedPath::File {
                            path: path.to_owned(),
                            handle: file,
                        };
                    } else if path.is_dir() {
                        self.open_files[i] = OpenedPath::Directory {
                            path: path.to_owned(),
                            entries: read_dir(path)?,
                            read_index: 0,
                        };
                    } else {
                        return None;
                    }
                    Some(())
                }();
            }

            if written_to!(file, i, read) {
                let bytes_read = || -> Option<u16> {
                    match &mut self.open_files[i] {
                        OpenedPath::File { path: _, handle } => handle
                            .read(cpu.slice_mut(
                                self.io_memory.file[i].read.get(),
                                self.io_memory.file[i].get_operation_length(),
                            )?)
                            .ok()
                            .map(|x| x as u16),
                        OpenedPath::Directory {
                            path: _,
                            entries,
                            read_index,
                        } => {
                            let max_length = self.io_memory.file[i].get_operation_length() as usize;
                            let mut entries_read = vec![];
                            while *read_index < entries.len() {
                                let bytes_to_append = entries[*read_index].len() + 1;
                                if bytes_to_append + entries_read.len() > max_length {
                                    break;
                                }
                                entries_read.extend(&entries[*read_index]);
                                entries_read.push('\n' as u8);
                                *read_index += 1;
                            }
                            cpu.slice_mut(
                                self.io_memory.file[i].read.get(),
                                entries_read.len() as u16,
                            )?
                            .copy_from_slice(&entries_read);
                            Some(entries_read.len() as u16)
                        }
                        _ => None,
                    }
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success.set(bytes_read);
            }

            if written_to!(file, i, write) {
                let bytes_written = || -> Option<u16> {
                    match &mut self.open_files[i] {
                        OpenedPath::File { path: _, handle } => {
                            let length = self.io_memory.file[i].get_operation_length();
                            let src = cpu.slice(self.io_memory.file[i].write.get(), length)?;
                            return handle.write(src).map(|x| x as u16).ok();
                        }
                        _ => None,
                    }
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success.set(bytes_written);
            }

            if written_to!(file, i, stat) {
                let bytes_written = || -> Option<u16> {
                    let entry = read_dir_entry(match &self.open_files[i] {
                        OpenedPath::File { path, handle: _ } => Some(path),
                        OpenedPath::Directory {
                            path,
                            entries: _,
                            read_index: __,
                        } => Some(path),
                        OpenedPath::None => None,
                    }?)?;
                    let length = std::cmp::min(
                        self.io_memory.file[i].get_operation_length(),
                        entry.len() as u16,
                    );
                    let dst = cpu.slice_mut(self.io_memory.file[i].stat.get(), length)?;
                    dst.copy_from_slice(&entry);
                    Some(length)
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success.set(bytes_written);
            }

            if written_to!(file, i, delete) && self.io_memory.file[i].delete != 0 {
                match &mut self.open_files[i] {
                    OpenedPath::File { path, handle: _ } => {
                        if std::fs::remove_file(&path).is_err() {
                            eprintln!("Could not remove {:?}", path);
                        }
                    }
                    OpenedPath::Directory {
                        path,
                        entries: _,
                        read_index: __,
                    } => {
                        if std::fs::remove_dir(&path).is_err() {
                            eprintln!("Could not remove {:?}", path);
                        }
                    }
                    OpenedPath::None => {}
                }

                self.open_files[i] = Default::default();
            }
        }

        if written_to!(system, red) || written_to!(system, green) || written_to!(system, blue) {
            self.frame.set_palette(
                self.io_memory.system.red.get(),
                self.io_memory.system.green.get(),
                self.io_memory.system.blue.get(),
            );
        }

        if written_to!(screen, width) || written_to!(screen, height) {
            let width = self.io_memory.screen.width.get();
            let height = self.io_memory.screen.height.get();
            self.frame.resize(width, height);
        }

        if written_to!(screen, pixel) {
            self.io_memory.screen.draw_pixel(&mut self.frame);
        }

        if written_to!(screen, sprite) {
            self.io_memory.screen.draw_sprite(cpu, &mut self.frame);
        }

        if written_to!(system, expansion) {
            ExpansionCommand::perform(cpu, self, self.io_memory.system.expansion.get());
        }

        if written_to!(system, friend) {
            let entry_point = self.io_memory.system.friend.get();
            if entry_point != 0 {
                // TODO: Don't copy ram between instances
                let mut friend = uxn::Uxn::boot(cpu.slice(0, u16::MAX).unwrap());
                eval_with_fault_handling(&mut friend, self, entry_point);
                cpu.slice_mut(0, u16::MAX)
                    .unwrap()
                    .copy_from_slice(friend.slice(0, u16::MAX).unwrap());
            }
        }

        if written_to!(system, metadata) {
            self.metadata_update =
                ProgramMetadata::read_from_uxn(cpu, self.io_memory.system.metadata.get());
        }

        for i in 0..self.io_memory.audio.len() {
            if written_to!(audio, i, pitch) {
                // TODO: Play sample
            }
        }

        Some(())
    }
}

fn eval_with_fault_handling(vm: &mut uxn::Uxn, host: &mut Varvara, entry_point: u16) {
    match vm.eval(host, entry_point) {
        uxn::UxnEvalResult::Ok => {}
        uxn::UxnEvalResult::Fault {
            where_the_error_occured,
            instruction_that_faulted,
            error_code,
        } => {
            let fault_handler = host.io_memory.system.halt.get();
            if fault_handler != 0 {
                // Empty the stacks
                vm.working_stack.head = 0;
                vm.return_stack.head = 0;
                vm.working_stack.push16(where_the_error_occured).unwrap();
                vm.working_stack.push8(instruction_that_faulted).unwrap();
                vm.working_stack.push8(error_code.clone() as u8).unwrap();
                match vm.eval(host, fault_handler) {
                    uxn::UxnEvalResult::Fault {
                        where_the_error_occured,
                        instruction_that_faulted,
                        error_code,
                    } => {
                        panic!(
                            "Failed with {:?} ({:02x}) at {:04x} ({:02x}) during fault handling",
                            error_code.clone(),
                            error_code.clone() as u8,
                            where_the_error_occured,
                            instruction_that_faulted
                        );
                    }
                    _ => {}
                }
            } else {
                panic!(
                    "Failed with {:?} ({:02x}) at {:04x} ({:02x})",
                    error_code.clone(),
                    error_code.clone() as u8,
                    where_the_error_occured,
                    instruction_that_faulted
                );
            }
        }
    }
}

fn inject_console_byte(vm: &mut uxn::Uxn, host: &mut Varvara, byte: u8, kind: ConsoleType) {
    host.io_memory.console.console_type = kind;
    host.io_memory.console.read = byte;
    let entry = host.io_memory.console.vector.get();
    eval_with_fault_handling(vm, host, entry);
}

const WIDTH: u32 = 64 * 8;
const HEIGHT: u32 = 40 * 8;
const PAD: u32 = 2;
const ZOOM: u32 = 2; // TODO: Parse from args

fn redraw<'a>(
    uxn_screen: &'a mut Frame,
    render_destination: &mut sdl2::rect::Rect,
    renderer: &mut sdl2::render::WindowCanvas,
    metadata: &mut Option<ProgramMetadata>,
) {
    if metadata.is_some() {
        let metadata_ref: &ProgramMetadata = metadata.as_ref().unwrap();
        if metadata_ref.text.len() != 0 {
            renderer
                .window_mut()
                .set_title(&metadata_ref.text[0])
                .unwrap();
        }

        *metadata = None;
    }

    if render_destination.w as usize != uxn_screen.width
        || render_destination.h as usize != uxn_screen.height
    {
        render_destination.x = PAD as i32;
        render_destination.y = PAD as i32;
        render_destination.w = uxn_screen.width as i32;
        render_destination.h = uxn_screen.height as i32;

        let padded_width = (uxn_screen.width + ((PAD as usize) * 2)) as u32;
        let padded_height = (uxn_screen.height + ((PAD as usize) * 2)) as u32;

        renderer
            .set_logical_size(padded_width, padded_height)
            .unwrap(); // TODO: Fault vm when it fails

        renderer
            .window_mut()
            .set_size(padded_width * ZOOM, padded_height * ZOOM)
            .unwrap(); // TODO: Fault vm when it fails

        // TODO: Create texture cache here?
    }

    // TODO: Don't allocate a new texture each render
    let texture_creator = renderer.texture_creator();
    let mut frame_texture = texture_creator
        .create_texture(
            sdl2::pixels::PixelFormatEnum::RGB888,
            sdl2::render::TextureAccess::Static,
            uxn_screen.width as u32,
            uxn_screen.height as u32,
        )
        .unwrap(); // TODO: Fault vm when it fails
    frame_texture.set_blend_mode(sdl2::render::BlendMode::None);

    uxn_screen.update_pixels();

    let pixel_data: &'a [u8] = unsafe {
        std::slice::from_raw_parts(
            std::mem::transmute(uxn_screen.pixels.as_ptr()),
            std::mem::size_of::<u32>() * uxn_screen.pixels.len(),
        )
    };
    frame_texture
        .update(
            None,
            pixel_data,
            uxn_screen.width * std::mem::size_of::<u32>(),
        )
        .unwrap(); // TODO: Fault vm when it fails

    renderer.clear();
    renderer
        .copy(&frame_texture, None, *render_destination)
        .unwrap(); // TODO: Fault vm when it fails
    renderer.present();
}

fn controller_button_from_sdl_keycode(key: sdl2::keyboard::Keycode) -> u8 {
    match key {
        sdl2::keyboard::Keycode::LCtrl | sdl2::keyboard::Keycode::RCtrl => Controller::A,
        sdl2::keyboard::Keycode::LAlt | sdl2::keyboard::Keycode::RAlt => Controller::B,
        sdl2::keyboard::Keycode::LShift
        | sdl2::keyboard::Keycode::RShift
        | sdl2::keyboard::Keycode::AcBack => Controller::SELECT,
        sdl2::keyboard::Keycode::Home | sdl2::keyboard::Keycode::AcHome => Controller::START,
        sdl2::keyboard::Keycode::Up => Controller::UP,
        sdl2::keyboard::Keycode::Down => Controller::DOWN,
        sdl2::keyboard::Keycode::Left => Controller::LEFT,
        sdl2::keyboard::Keycode::Right => Controller::RIGHT,
        _ => 0,
    }
}

fn inject_button_event(vm: &mut uxn::Uxn, host: &mut Varvara, button: u8, pressed_down: bool) {
    let entry = host.io_memory.controller.vector.get();
    if pressed_down {
        host.io_memory.controller.button |= button;
    } else {
        host.io_memory.controller.button &= !button;
    }
    eval_with_fault_handling(vm, host, entry);
}

fn controller_key_from_sdl_keycode(
    key: sdl2::keyboard::Keycode,
    keymod: sdl2::keyboard::Mod,
) -> u8 {
    // NOTE: Assuming the sdl2::keyboard::Keycode is ordered like ASCII

    if (key as i32) > u8::MAX as i32 {
        return 0;
    }

    let key_b = key as u8;
    if (key_b < ' ' as u8) || key == sdl2::keyboard::Keycode::Delete {
        return key_b;
    }

    if keymod.contains(sdl2::keyboard::Mod::LCTRLMOD)
        || keymod.contains(sdl2::keyboard::Mod::RCTRLMOD)
    {
        if key_b < 'a' as u8 {
            return key_b;
        } else if key_b <= 'z' as u8 {
            if keymod.contains(sdl2::keyboard::Mod::LSHIFTMOD)
                || keymod.contains(sdl2::keyboard::Mod::RSHIFTMOD)
            {
                return key_b - ('a' as u8 - 'A' as u8);
            } else {
                return key_b;
            }
        }
    }

    return 0;
}

fn inject_key_event(vm: &mut uxn::Uxn, host: &mut Varvara, key: u8) {
    host.io_memory.controller.key = key;
    let entry = host.io_memory.controller.vector.get();
    eval_with_fault_handling(vm, host, entry);
    host.io_memory.controller.key = 0;
}

fn inject_mouse_motion_event(vm: &mut uxn::Uxn, host: &mut Varvara, x: u16, y: u16) {
    let width = host.io_memory.screen.width.get();
    let x = std::cmp::min(x.saturating_sub(PAD as u16), width - 1);
    host.io_memory.mouse.x.set(x);

    let height = host.io_memory.screen.height.get();
    let y = std::cmp::min(y.saturating_sub(PAD as u16), height - 1);
    host.io_memory.mouse.y.set(y);

    let entry = host.io_memory.mouse.vector.get();
    eval_with_fault_handling(vm, host, entry);
}

fn mouse_button_from_sdl(button: sdl2::mouse::MouseButton) -> u8 {
    match button {
        sdl2::mouse::MouseButton::Left => Mouse::MOUSE1,
        sdl2::mouse::MouseButton::Right => Mouse::MOUSE2,
        sdl2::mouse::MouseButton::Middle => Mouse::MOUSE3,
        _ => 0,
    }
}

fn inject_mouse_button_event(
    vm: &mut uxn::Uxn,
    host: &mut Varvara,
    button: u8,
    pressed_down: bool,
) {
    if pressed_down {
        host.io_memory.mouse.state |= button;
    } else {
        host.io_memory.mouse.state &= !button;
    }

    let entry = host.io_memory.mouse.vector.get();
    eval_with_fault_handling(vm, host, entry);
}

fn inject_mouse_scroll_event(vm: &mut uxn::Uxn, host: &mut Varvara, x: i16, y: i16) {
    host.io_memory.mouse.scrollx.set_signed(x);
    host.io_memory.mouse.scrolly.set_signed(y);
    let entry = host.io_memory.mouse.vector.get();
    eval_with_fault_handling(vm, host, entry);
    host.io_memory.mouse.scrollx.set_signed(0);
    host.io_memory.mouse.scrolly.set_signed(0);
}

fn boot(mut rom: Vec<u8>) -> (uxn::Uxn, Varvara) {
    let mut host = Varvara::default();

    if rom.len() < uxn::MAX_ROM_SIZE {
        return (uxn::Uxn::boot(&rom), host);
    }

    let vm = uxn::Uxn::boot(&rom[0..uxn::MAX_ROM_SIZE]);
    host.rom_expansion = rom.split_off(u16::MAX as usize);
    return (vm, host);
}

fn main() {
    let mut args = std::env::args();
    let mut rom = vec![];
    let mut file =
        std::fs::File::open(args.nth(1).expect("Expected a rom path as an argument")).unwrap();
    file.read_to_end(&mut rom).unwrap();

    let sdl_context = sdl2::init().expect("Failed to initialize sdl2");
    let sdl_video_subsystem = sdl_context
        .video()
        .expect("Failed to start sdl2's video subsystem");
    let window = sdl_video_subsystem
        .window(
            "varvara",
            (WIDTH + PAD * 2) * ZOOM,
            (HEIGHT + PAD * 2) * ZOOM,
        )
        .allow_highdpi()
        .build()
        .unwrap();
    let mut renderer = window.into_canvas().build().unwrap();
    renderer.set_draw_color(sdl2::pixels::Color {
        r: 0,
        g: 0,
        b: 0,
        a: 0xff,
    });
    let mut render_destination = sdl2::rect::Rect::new(0, 0, 0, 0);
    sdl_context.mouse().show_cursor(false);

    let (mut vm, mut host) = boot(rom);

    host.deo(
        &mut vm,
        offset_of_device_port!(screen, width),
        WIDTH as u16,
        true,
    )
    .unwrap();

    host.deo(
        &mut vm,
        offset_of_device_port!(screen, height),
        HEIGHT as u16,
        true,
    )
    .unwrap();

    // Run initialization
    eval_with_fault_handling(&mut vm, &mut host, uxn::PAGE_PROGRAM as u16);
    redraw(
        &mut host.frame,
        &mut render_destination,
        &mut renderer,
        &mut host.metadata_update,
    );

    // Process arguments
    let args_len = args.len();
    for (i, arg) in args.enumerate() {
        for c in arg.as_bytes() {
            inject_console_byte(&mut vm, &mut host, *c, ConsoleType::Argument);
        }
        inject_console_byte(
            &mut vm,
            &mut host,
            '\n' as u8,
            if i == (args_len - 1) {
                ConsoleType::ArgumentEnd
            } else {
                ConsoleType::ArgumentSeparator
            },
        );
        redraw(
            &mut host.frame,
            &mut render_destination,
            &mut renderer,
            &mut host.metadata_update,
        );
    }

    let (stdin_tx, stdin_rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut byte = [0];
        loop {
            match std::io::stdin().read(&mut byte) {
                Ok(amount_read) => {
                    if amount_read != 0 {
                        match stdin_tx.send(byte[0]) {
                            Ok(_) => {}
                            _ => break,
                        }
                    }
                }
                _ => break,
            }
        }
    });

    let mut event_pump = sdl_context.event_pump().unwrap();
    const TARGET_FPS: u32 = 60;
    let max_sleep_duration: std::time::Duration = std::time::Duration::from_secs(1) / TARGET_FPS;
    let mut time_at_end_of_last_iteration = std::time::Instant::now();
    'sdl_loop: while host.io_memory.system.state == 0 {
        'stdin_read_loop: loop {
            match stdin_rx.try_recv() {
                Ok(byte) => {
                    inject_console_byte(&mut vm, &mut host, byte, ConsoleType::StdIn);
                }
                _ => break 'stdin_read_loop,
            }
        }

        let screen_vector = host.io_memory.screen.vector.get();
        if screen_vector != 0 {
            eval_with_fault_handling(&mut vm, &mut host, screen_vector);
        }

        redraw(
            &mut host.frame,
            &mut render_destination,
            &mut renderer,
            &mut host.metadata_update,
        );

        for event in event_pump.poll_iter() {
            match event {
                sdl2::event::Event::Quit { timestamp: _ } => break 'sdl_loop,
                sdl2::event::Event::KeyUp {
                    timestamp: _,
                    window_id: _,
                    keycode,
                    scancode: _,
                    keymod: _,
                    repeat,
                } => {
                    if keycode.is_some() && !repeat {
                        let button = controller_button_from_sdl_keycode(keycode.unwrap());
                        if button != 0 {
                            inject_button_event(&mut vm, &mut host, button, false);
                        }
                    }
                }
                sdl2::event::Event::KeyDown {
                    timestamp: _,
                    window_id: _,
                    keycode,
                    scancode: _,
                    keymod,
                    repeat,
                } => {
                    if keycode.is_some() {
                        let key = controller_key_from_sdl_keycode(keycode.unwrap(), keymod);
                        if key != 0 {
                            inject_key_event(&mut vm, &mut host, key);
                        } else if !repeat {
                            let button = controller_button_from_sdl_keycode(keycode.unwrap());
                            if button != 0 {
                                inject_button_event(&mut vm, &mut host, button, true);
                            }
                        }
                    }
                }
                sdl2::event::Event::TextInput {
                    timestamp: _,
                    window_id: _,
                    text,
                } => {
                    for b in text.as_bytes() {
                        inject_key_event(&mut vm, &mut host, *b);
                    }
                }
                sdl2::event::Event::MouseMotion {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    mousestate: _,
                    x,
                    y,
                    xrel: _,
                    yrel: _,
                } => {
                    inject_mouse_motion_event(&mut vm, &mut host, x as u16, y as u16);
                }
                sdl2::event::Event::MouseButtonDown {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    mouse_btn,
                    clicks: _,
                    x: _,
                    y: _,
                } => {
                    let button = mouse_button_from_sdl(mouse_btn);
                    if button != 0 {
                        inject_mouse_button_event(&mut vm, &mut host, button, true);
                    }
                }
                sdl2::event::Event::MouseButtonUp {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    mouse_btn,
                    clicks: _,
                    x: _,
                    y: _,
                } => {
                    let button = mouse_button_from_sdl(mouse_btn);
                    if button != 0 {
                        inject_mouse_button_event(&mut vm, &mut host, button, false);
                    }
                }
                sdl2::event::Event::MouseWheel {
                    timestamp: _,
                    window_id: _,
                    which: _,
                    x,
                    y,
                    direction: _,
                } => {
                    inject_mouse_scroll_event(&mut vm, &mut host, x as i16, -(y as i16));
                }

                // TODO: sdl2::event::Event::JoyButtonUp
                // TODO: sdl2::event::Event::JoyButtonDown
                // TODO: sdl2::event::Event::JoyHatMotion
                // TODO: sdl2::event::Event::JoyAxisMotion
                // TODO: sdl2::event::Event::JoyBallMotion
                // TODO: sdl2::event::Event::ControllerAxisMotion
                // TODO: sdl2::event::Event::TextEditing ?
                // TODO: sdl2::event::Event::FingerDown
                // TODO: sdl2::event::Event::FingerUp
                // TODO: sdl2::event::Event::FingerMotion
                _ => {}
            }
        }

        let time_since_last_iteration = std::time::Instant::now() - time_at_end_of_last_iteration;
        if time_since_last_iteration < max_sleep_duration {
            let duration_to_sleep = max_sleep_duration - time_since_last_iteration;
            std::thread::sleep(duration_to_sleep);
        }

        // TODO: Implement iterator so if a `continue` is used before this line, than things won't break
        time_at_end_of_last_iteration = std::time::Instant::now();
    }
}

// TODO: Move to another file
#[cfg(test)]
mod sandbox_tests {
    use super::complies_with_sandbox_rules;

    fn with_tempdir<F>(function: F)
    where
        F: FnOnce() -> () + std::panic::UnwindSafe,
    {
        let test_result;

        {
            static CURRENT_DIR_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());
            let _currect_dir_guard = CURRENT_DIR_MUTEX.lock().unwrap();
            let current_dir_backup = std::env::current_dir().unwrap();

            let test_directory = std::env::temp_dir().join("uxncli-sandbox-tests");
            assert!(!test_directory.exists());
            std::fs::create_dir(&test_directory).unwrap();

            let working_directory = test_directory.as_path().join("working_dir");
            assert!(!working_directory.exists());
            std::fs::create_dir(&working_directory).unwrap();

            std::env::set_current_dir(&working_directory).unwrap();
            test_result = std::panic::catch_unwind(|| function());
            std::env::set_current_dir(current_dir_backup).unwrap();
            std::fs::remove_dir_all(&test_directory).unwrap();
        }

        assert!(test_result.is_ok());
    }

    #[test]
    fn test_file_in_current_working_directory() {
        with_tempdir(|| {
            let file_name = "test.txt";
            std::fs::File::create(file_name).unwrap();
            assert!(complies_with_sandbox_rules(std::path::Path::new(file_name)));
        });
    }

    #[test]
    fn test_file_outside_current_working_directory() {
        with_tempdir(|| {
            let file_name = std::path::PathBuf::from("../test.txt");
            std::fs::File::create(&file_name).unwrap();
            assert!(!complies_with_sandbox_rules(&file_name));
        });
    }

    #[test]
    fn test_file_in_subdirectory() {
        with_tempdir(|| {
            let dir_path = std::path::PathBuf::from("dir");
            std::fs::create_dir(&dir_path).unwrap();
            let file_path = dir_path.join("file.txt");
            std::fs::File::create(file_path.as_path()).unwrap();
            assert!(complies_with_sandbox_rules(file_path.as_path()));
        });
    }

    #[cfg(target_family = "unix")]
    #[test]
    fn test_symlink_that_points_to_something_in_the_working_directory() {
        with_tempdir(|| {
            let file1_path = std::path::PathBuf::from("file1.txt");
            std::fs::File::create(file1_path.as_path()).unwrap();
            let file2_path = std::path::PathBuf::from("file2.txt");
            std::os::unix::fs::symlink(file1_path.as_path(), file2_path.as_path()).unwrap();
            assert!(!complies_with_sandbox_rules(file2_path.as_path()));
        });
    }

    #[cfg(target_family = "unix")]
    #[test]
    fn test_symlink_that_points_to_something_outside_the_working_directory() {
        with_tempdir(|| {
            let file1_path = std::path::PathBuf::from("../file1.txt");
            std::fs::File::create(file1_path.as_path()).unwrap();
            let file2_path = std::path::PathBuf::from("file2.txt");
            std::os::unix::fs::symlink(file1_path.as_path(), file2_path.as_path()).unwrap();
            assert!(!complies_with_sandbox_rules(file2_path.as_path()));
        });
    }

    #[test]
    fn test_file_that_doesnt_exist_in_current_working_directory() {
        with_tempdir(|| {
            let file_path = std::path::PathBuf::from("file1.txt");
            assert!(!file_path.exists());
            assert!(complies_with_sandbox_rules(file_path.as_path()));
        });
    }

    #[test]
    fn test_file_that_doesnt_exist_outside_of_current_working_directory() {
        with_tempdir(|| {
            let file1_path = std::path::PathBuf::from("../file1.txt");
            assert!(!file1_path.exists());
            let file2_path = std::path::PathBuf::from("file2.txt");
            std::os::unix::fs::symlink(file1_path.as_path(), file2_path.as_path()).unwrap();
            assert!(!complies_with_sandbox_rules(file2_path.as_path()));
        });
    }
}
