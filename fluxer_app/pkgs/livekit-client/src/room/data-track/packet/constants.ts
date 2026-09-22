// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export const U8_LENGTH_BYTES = 1;
export const U16_LENGTH_BYTES = 2;
export const U32_LENGTH_BYTES = 4;
export const U64_LENGTH_BYTES = 8;

export const SUPPORTED_VERSION = 0;
export const BASE_HEADER_LEN = 12;

export const VERSION_SHIFT = 5;
export const VERSION_MASK = 0x07;

export const FRAME_MARKER_SHIFT = 3;
export const FRAME_MARKER_MASK = 0x3;

export const FRAME_MARKER_START = 0x2;
export const FRAME_MARKER_FINAL = 0x1;
export const FRAME_MARKER_INTER = 0x0;
export const FRAME_MARKER_SINGLE = 0x3;

export const EXT_WORDS_INDICATOR_SIZE = 2;
export const EXT_FLAG_SHIFT = 0x2;
export const EXT_FLAG_MASK = 0x1;
export const EXT_MARKER_LEN = 4;
export const EXT_TAG_PADDING = 0;
