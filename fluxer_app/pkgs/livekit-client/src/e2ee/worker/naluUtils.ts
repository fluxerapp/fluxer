// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';

const kH264NaluTypeMask = 0x1f;
const kH264SliceClearBytes = 2;
const kH265SliceClearBytes = 3;

enum H264NALUType {
	SLICE_NON_IDR = 1,
	SLICE_PARTITION_A = 2,
	SLICE_PARTITION_B = 3,
	SLICE_PARTITION_C = 4,
	SLICE_IDR = 5,
	SEI = 6,
	SPS = 7,
	PPS = 8,
	AUD = 9,
	END_SEQ = 10,
	END_STREAM = 11,
	FILLER_DATA = 12,
	SPS_EXT = 13,
	PREFIX_NALU = 14,
	SUBSET_SPS = 15,
	DPS = 16,

	SLICE_AUX = 19,
	SLICE_EXT = 20,
	SLICE_LAYER_EXT = 21,
}

enum H265NALUType {
	TRAIL_N = 0,
	TRAIL_R = 1,
	TSA_N = 2,
	TSA_R = 3,
	STSA_N = 4,
	STSA_R = 5,
	RADL_N = 6,
	RADL_R = 7,
	RASL_N = 8,
	RASL_R = 9,

	BLA_W_LP = 16,
	BLA_W_RADL = 17,
	BLA_N_LP = 18,
	IDR_W_RADL = 19,
	IDR_N_LP = 20,
	CRA_NUT = 21,

	VPS_NUT = 32,
	SPS_NUT = 33,
	PPS_NUT = 34,
	AUD_NUT = 35,
	EOS_NUT = 36,
	EOB_NUT = 37,
	FD_NUT = 38,
	PREFIX_SEI_NUT = 39,
	SUFFIX_SEI_NUT = 40,
}

function parseH264NALUType(startByte: number): H264NALUType {
	return startByte & kH264NaluTypeMask;
}

function parseH265NALUType(firstByte: number): H265NALUType {
	return (firstByte >> 1) & 0x3f;
}

function isH264SliceNALU(naluType: H264NALUType): boolean {
	return naluType === H264NALUType.SLICE_IDR || naluType === H264NALUType.SLICE_NON_IDR;
}

function isH265SliceNALU(naluType: H265NALUType): boolean {
	return (
		naluType === H265NALUType.TRAIL_N ||
		naluType === H265NALUType.TRAIL_R ||
		naluType === H265NALUType.TSA_N ||
		naluType === H265NALUType.TSA_R ||
		naluType === H265NALUType.STSA_N ||
		naluType === H265NALUType.STSA_R ||
		naluType === H265NALUType.RADL_N ||
		naluType === H265NALUType.RADL_R ||
		naluType === H265NALUType.RASL_N ||
		naluType === H265NALUType.RASL_R ||
		naluType === H265NALUType.BLA_W_LP ||
		naluType === H265NALUType.BLA_W_RADL ||
		naluType === H265NALUType.BLA_N_LP ||
		naluType === H265NALUType.IDR_W_RADL ||
		naluType === H265NALUType.IDR_N_LP ||
		naluType === H265NALUType.CRA_NUT
	);
}

export type DetectedCodec = 'h264' | 'h265' | 'unknown';

export interface NALUProcessingResult {
	unencryptedBytes: number;
	detectedCodec: DetectedCodec;
	requiresNALUProcessing: boolean;
}

function detectCodecFromNALUs(data: NonSharedUint8Array, naluIndices: Array<number>): DetectedCodec {
	for (const naluIndex of naluIndices) {
		if (isH264SliceNALU(parseH264NALUType(data[naluIndex]))) return 'h264';
		if (isH265SliceNALU(parseH265NALUType(data[naluIndex]))) return 'h265';
	}
	return 'unknown';
}

function findSliceNALUUnencryptedBytes(
	data: NonSharedUint8Array,
	naluIndices: Array<number>,
	codec: 'h264' | 'h265',
): number | null {
	for (const index of naluIndices) {
		if (codec === 'h265') {
			const type = parseH265NALUType(data[index]);
			if (isH265SliceNALU(type)) {
				return Math.min(index + kH265SliceClearBytes, data.length);
			}
		} else {
			const type = parseH264NALUType(data[index]);
			if (isH264SliceNALU(type)) {
				return Math.min(index + kH264SliceClearBytes, data.length);
			}
		}
	}
	return null;
}

function findNALUIndices(stream: NonSharedUint8Array): Array<number> {
	const result: Array<number> = [];
	let start = 0,
		pos = 0,
		searchLength = stream.length - 3;

	while (pos < searchLength) {
		while (pos < searchLength) {
			if (
				pos < searchLength - 1 &&
				stream[pos] === 0 &&
				stream[pos + 1] === 0 &&
				stream[pos + 2] === 0 &&
				stream[pos + 3] === 1
			) {
				break;
			}
			if (stream[pos] === 0 && stream[pos + 1] === 0 && stream[pos + 2] === 1) {
				break;
			}
			pos++;
		}

		if (pos >= searchLength) pos = stream.length;

		let end = pos;
		while (end > start && stream[end - 1] === 0) end--;

		if (start === 0) {
			if (end !== start) throw TypeError('byte stream contains leading data');
		} else {
			result.push(start);
		}

		let startCodeLength = 3;
		if (
			pos < stream.length - 3 &&
			stream[pos] === 0 &&
			stream[pos + 1] === 0 &&
			stream[pos + 2] === 0 &&
			stream[pos + 3] === 1
		) {
			startCodeLength = 4;
		}

		start = pos = pos + startCodeLength;
	}
	return result;
}

export function processNALUsForEncryption(
	data: NonSharedUint8Array,
	knownCodec?: 'h264' | 'h265',
): NALUProcessingResult {
	const naluIndices = findNALUIndices(data);
	const detectedCodec = knownCodec ?? detectCodecFromNALUs(data, naluIndices);

	if (detectedCodec === 'unknown') {
		return {unencryptedBytes: 0, detectedCodec, requiresNALUProcessing: false};
	}

	const unencryptedBytes = findSliceNALUUnencryptedBytes(data, naluIndices, detectedCodec);
	if (unencryptedBytes === null) {
		throw new TypeError('Could not find NALU');
	}

	return {unencryptedBytes, detectedCodec, requiresNALUProcessing: true};
}
