// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export const GCM_TAG_LENGTH_BYTES = 16;

export const AV1_E2EE_METADATA_OBU_TYPE = 5;

const AV1_E2EE_METADATA_MAGIC_0 = 0x4c;
const AV1_E2EE_METADATA_MAGIC_1 = 0x4b;
const AV1_E2EE_METADATA_VERSION = 1;

type ByteRange = {start: number; end: number};

export interface Av1EncryptionLayout {
	protectedRanges: Array<ByteRange>;
	protectedLength: number;
	aadRanges: Array<ByteRange>;
	aadLength: number;
	buildAAD: (data: Uint8Array) => Uint8Array;
	extractProtected: (data: Uint8Array) => Uint8Array;
	writeProtected: (target: Uint8Array, protectedBytes: Uint8Array) => void;
}

type Av1EncryptionLayoutBase = Omit<Av1EncryptionLayout, 'buildAAD' | 'extractProtected' | 'writeProtected'>;

type Av1LayoutParser = (data: Uint8Array) => Av1EncryptionLayoutBase | undefined;

function isValidRange(range: ByteRange, dataLength: number): boolean {
	return range.start >= 0 && range.end >= range.start && range.end <= dataLength;
}

function buildByteArrayFromRanges(data: Uint8Array, ranges: Array<ByteRange>, totalLength: number) {
	const out = new Uint8Array(totalLength);
	let writeOffset = 0;
	for (const range of ranges) {
		out.set(data.subarray(range.start, range.end), writeOffset);
		writeOffset += range.end - range.start;
	}
	return out;
}

function writeByteArrayIntoRanges(
	target: Uint8Array,
	ranges: Array<ByteRange>,
	source: Uint8Array,
	totalLength: number,
) {
	if (source.byteLength !== totalLength) {
		throw new Error(`Unexpected protected bytes length: ${source.byteLength}, expected ${totalLength}`);
	}
	let readOffset = 0;
	for (const range of ranges) {
		const len = range.end - range.start;
		target.set(source.subarray(readOffset, readOffset + len), range.start);
		readOffset += len;
	}
}

function readLeb128(data: Uint8Array, offset: number): {value: number; length: number} | undefined {
	let value = 0;
	let shift = 0;
	let length = 0;

	while (offset + length < data.length) {
		const byte = data[offset + length];
		value += (byte & 0x7f) * 2 ** shift;
		length++;

		if ((byte & 0x80) === 0) {
			return {value, length};
		}

		shift += 7;
		if (length >= 5) return undefined;
	}

	return undefined;
}

function parseObuHeader(byte: number): {
	obuType: number;
	extensionFlag: boolean;
	hasSizeField: boolean;
} | null {
	if ((byte & 0x80) !== 0) return null;
	if ((byte & 0x01) !== 0) return null;

	const obuType = (byte & 0x78) >> 3;
	const extensionFlag = (byte & 0x04) !== 0;
	const hasSizeField = (byte & 0x02) !== 0;
	return {obuType, extensionFlag, hasSizeField};
}

export interface Av1E2eeMetadata {
	keyIndex: number;
	iv: Uint8Array;
	tag: Uint8Array;
}

const _metadataObuHeaderByte = (AV1_E2EE_METADATA_OBU_TYPE << 3) | 0x02;
const _metadataObuPayloadSize = 32;
const _metadataObuTotalSize = 1 + 1 + _metadataObuPayloadSize;

export function buildAv1E2eeMetadataObu(meta: Av1E2eeMetadata): Uint8Array {
	const out = new Uint8Array(_metadataObuTotalSize);
	out[0] = _metadataObuHeaderByte;
	out[1] = _metadataObuPayloadSize;
	out[2] = AV1_E2EE_METADATA_MAGIC_0;
	out[3] = AV1_E2EE_METADATA_MAGIC_1;
	out[4] = AV1_E2EE_METADATA_VERSION;
	out[5] = meta.keyIndex & 0xff;
	out.set(meta.iv, 6);
	out.set(meta.tag, 18);
	return out;
}

function readAv1E2eeMetadataPayload(payload: Uint8Array): Av1E2eeMetadata | undefined {
	if (payload.byteLength !== _metadataObuPayloadSize) return undefined;
	if (
		payload[0] !== AV1_E2EE_METADATA_MAGIC_0 ||
		payload[1] !== AV1_E2EE_METADATA_MAGIC_1 ||
		payload[2] !== AV1_E2EE_METADATA_VERSION
	) {
		return undefined;
	}

	return {
		keyIndex: payload[3],
		iv: payload.subarray(4, 16),
		tag: payload.subarray(16, 32),
	};
}

function extractAv1E2eeMetadataObuFromTail(data: Uint8Array): {payload: Uint8Array; meta: Av1E2eeMetadata} | undefined {
	if (data.byteLength < _metadataObuTotalSize) return undefined;
	const obuStart = data.byteLength - _metadataObuTotalSize;
	if (data[obuStart] !== _metadataObuHeaderByte || data[obuStart + 1] !== _metadataObuPayloadSize) {
		return undefined;
	}
	const meta = readAv1E2eeMetadataPayload(data.subarray(obuStart + 2));
	if (!meta) return undefined;
	return {
		payload: data.subarray(0, obuStart),
		meta,
	};
}

export function extractAv1E2eeMetadataObu(data: Uint8Array): {payload: Uint8Array; meta: Av1E2eeMetadata} | undefined {
	const tailMetadata = extractAv1E2eeMetadataObuFromTail(data);
	if (tailMetadata) return tailMetadata;

	let offset = 0;
	while (offset < data.length) {
		const obuStart = offset;
		const header = parseObuHeader(data[offset]);
		if (!header) return undefined;

		const headerLen = 1 + (header.extensionFlag ? 1 : 0);
		if (offset + headerLen > data.length) return undefined;

		if (!header.hasSizeField) return undefined;
		const leb = readLeb128(data, offset + headerLen);
		if (!leb) return undefined;
		const payloadLen = leb.value;
		const sizeFieldLen = leb.length;

		const payloadStart = offset + headerLen + sizeFieldLen;
		const payloadEnd = payloadStart + payloadLen;
		if (payloadEnd > data.length) return undefined;

		const obuEnd = payloadEnd;

		if (header.obuType === AV1_E2EE_METADATA_OBU_TYPE && obuEnd === data.length && payloadLen === 32) {
			const meta = readAv1E2eeMetadataPayload(data.subarray(payloadStart, payloadEnd));
			if (!meta) return undefined;
			return {
				payload: data.subarray(0, obuStart),
				meta,
			};
		}

		offset = obuEnd;
	}

	return undefined;
}

function shouldKeepFirstPayloadByteClear(obuType: number): boolean {
	return obuType === 3 || obuType === 6;
}

function computeLayoutFromRtpPayload(data: Uint8Array): Av1EncryptionLayoutBase | undefined {
	if (data.length < 2) return undefined;

	const aggregationHeader = data[0];
	if ((aggregationHeader & 0x07) !== 0) return undefined;
	const z = (aggregationHeader & 0x80) !== 0;
	const w = (aggregationHeader & 0x30) >> 4;

	const aadRanges: Array<ByteRange> = [{start: 0, end: 1}];
	const protectedRanges: Array<ByteRange> = [];
	let protectedLength = 0;
	let aadLength = 1;

	let offset = 1;
	let obuIndex = 0;
	while (offset < data.length) {
		const isLastObuElement = w > 0 ? obuIndex + 1 === w : false;

		let obuStart = offset;
		let obuEnd = data.length;

		if (!isLastObuElement) {
			const leb = readLeb128(data, offset);
			if (!leb) return undefined;
			const obuLen = leb.value;
			const lenFieldEnd = offset + leb.length;
			obuStart = lenFieldEnd;
			obuEnd = obuStart + obuLen;
			if (obuEnd > data.length) return undefined;

			aadRanges.push({start: offset, end: obuStart});
			aadLength += obuStart - offset;
		}

		if (obuStart >= obuEnd) {
			offset = obuEnd;
			obuIndex++;
			if (w > 0 && obuIndex >= w) break;
			continue;
		}

		if (z && obuIndex === 0) {
			protectedRanges.push({start: obuStart, end: obuEnd});
			protectedLength += obuEnd - obuStart;
			offset = obuEnd;
			obuIndex++;
			if (w > 0 && obuIndex >= w) break;
			continue;
		}

		const header = parseObuHeader(data[obuStart]);
		if (!header) return undefined;

		const headerLen = 1 + (header.extensionFlag ? 1 : 0);
		if (obuStart + headerLen > obuEnd) return undefined;

		let sizeFieldLen = 0;
		if (header.hasSizeField) {
			const lebInner = readLeb128(data, obuStart + headerLen);
			if (!lebInner) return undefined;
			sizeFieldLen = lebInner.length;
			if (obuStart + headerLen + sizeFieldLen > obuEnd) return undefined;
		}

		const prefixStart = obuStart;
		const prefixEnd = obuStart + headerLen + sizeFieldLen;
		if (prefixEnd > prefixStart) {
			aadRanges.push({start: prefixStart, end: prefixEnd});
			aadLength += prefixEnd - prefixStart;
		}

		const payloadStart = prefixEnd;
		const payloadEnd = obuEnd;
		const payloadLen = payloadEnd - payloadStart;

		const clearPayloadPrefixLen = shouldKeepFirstPayloadByteClear(header.obuType) ? Math.min(1, payloadLen) : 0;

		if (clearPayloadPrefixLen > 0) {
			aadRanges.push({start: payloadStart, end: payloadStart + clearPayloadPrefixLen});
			aadLength += clearPayloadPrefixLen;
		}

		const protectedStart = payloadStart + clearPayloadPrefixLen;
		if (protectedStart < payloadEnd) {
			protectedRanges.push({start: protectedStart, end: payloadEnd});
			protectedLength += payloadEnd - protectedStart;
		}

		offset = obuEnd;
		obuIndex++;
		if (w > 0 && obuIndex >= w) break;
	}

	return {protectedRanges, protectedLength, aadRanges, aadLength};
}

function computeLayoutFromRtxPayload(data: Uint8Array): Av1EncryptionLayoutBase | undefined {
	if (data.length < 3) return undefined;

	const inner = computeLayoutFromRtpPayload(data.subarray(2));
	if (!inner) return undefined;

	const aadRanges: Array<ByteRange> = [{start: 0, end: 2}];
	for (let i = 0; i < inner.aadRanges.length; i++) {
		const r = inner.aadRanges[i];
		aadRanges.push({start: r.start + 2, end: r.end + 2});
	}
	const protectedRanges: Array<ByteRange> = [];
	for (let i = 0; i < inner.protectedRanges.length; i++) {
		const r = inner.protectedRanges[i];
		protectedRanges.push({start: r.start + 2, end: r.end + 2});
	}
	const aadLength = inner.aadLength + 2;
	const protectedLength = inner.protectedLength;

	return {protectedRanges, protectedLength, aadRanges, aadLength};
}

function computeLayoutFromSizeFieldObuStream(data: Uint8Array): Av1EncryptionLayoutBase | undefined {
	let offset = 0;
	const aadRanges: Array<ByteRange> = [];
	const protectedRanges: Array<ByteRange> = [];
	let protectedLength = 0;
	let aadLength = 0;

	while (offset < data.length) {
		const header = parseObuHeader(data[offset]);
		if (!header) return undefined;

		const headerLen = 1 + (header.extensionFlag ? 1 : 0);
		if (offset + headerLen > data.length) return undefined;

		let sizeFieldLen = 0;
		let payloadLen = 0;
		if (header.hasSizeField) {
			const leb = readLeb128(data, offset + headerLen);
			if (!leb) return undefined;
			payloadLen = leb.value;
			sizeFieldLen = leb.length;
			if (offset + headerLen + sizeFieldLen + payloadLen > data.length) return undefined;
		} else {
			payloadLen = data.length - (offset + headerLen);
			sizeFieldLen = 0;
		}

		const prefixStart = offset;
		const prefixEnd = offset + headerLen + sizeFieldLen;
		if (prefixEnd > prefixStart) {
			aadRanges.push({start: prefixStart, end: prefixEnd});
			aadLength += prefixEnd - prefixStart;
		}

		const payloadStart = prefixEnd;
		const payloadEnd = payloadStart + payloadLen;
		const clearPayloadPrefixLen = shouldKeepFirstPayloadByteClear(header.obuType) ? Math.min(1, payloadLen) : 0;

		if (clearPayloadPrefixLen > 0) {
			aadRanges.push({start: payloadStart, end: payloadStart + clearPayloadPrefixLen});
			aadLength += clearPayloadPrefixLen;
		}

		const protectedStart = payloadStart + clearPayloadPrefixLen;
		if (protectedStart < payloadEnd) {
			protectedRanges.push({start: protectedStart, end: payloadEnd});
			protectedLength += payloadEnd - protectedStart;
		}

		offset = payloadEnd;
		if (!header.hasSizeField) break;
	}

	return {protectedRanges, protectedLength, aadRanges, aadLength};
}

function computeLayoutFromAnnexB(data: Uint8Array): Av1EncryptionLayoutBase | undefined {
	let offset = 0;
	const aadRanges: Array<ByteRange> = [];
	const protectedRanges: Array<ByteRange> = [];
	let protectedLength = 0;
	let aadLength = 0;

	while (offset < data.length) {
		const leb = readLeb128(data, offset);
		if (!leb) return undefined;

		const obuLen = leb.value;
		const lenFieldEnd = offset + leb.length;
		const obuStart = lenFieldEnd;
		const obuEnd = obuStart + obuLen;
		if (obuEnd > data.length) return undefined;

		const lenFieldSpan = obuStart - offset;
		aadRanges.push({start: offset, end: obuStart});
		aadLength += lenFieldSpan;

		if (obuLen === 0) {
			offset = obuEnd;
			continue;
		}

		const header = parseObuHeader(data[obuStart]);
		if (!header) return undefined;

		const headerLen = 1 + (header.extensionFlag ? 1 : 0);
		if (obuStart + headerLen > obuEnd) return undefined;

		let sizeFieldLen = 0;
		let payloadStart = obuStart + headerLen;
		if (header.hasSizeField) {
			const lebInner = readLeb128(data, payloadStart);
			if (!lebInner) return undefined;
			sizeFieldLen = lebInner.length;
			payloadStart += sizeFieldLen;
			if (payloadStart > obuEnd) return undefined;
		}

		const prefixStart = obuStart;
		const prefixEnd = obuStart + headerLen + sizeFieldLen;
		aadRanges.push({start: prefixStart, end: prefixEnd});
		aadLength += prefixEnd - prefixStart;

		const payloadEnd = obuEnd;
		const payloadLen = payloadEnd - payloadStart;

		const clearPayloadPrefixLen = shouldKeepFirstPayloadByteClear(header.obuType) ? Math.min(1, payloadLen) : 0;

		if (clearPayloadPrefixLen > 0) {
			aadRanges.push({start: payloadStart, end: payloadStart + clearPayloadPrefixLen});
			aadLength += clearPayloadPrefixLen;
		}

		const protectedStart = payloadStart + clearPayloadPrefixLen;
		if (protectedStart < payloadEnd) {
			protectedRanges.push({start: protectedStart, end: payloadEnd});
			protectedLength += payloadEnd - protectedStart;
		}

		offset = obuEnd;
	}

	return {protectedRanges, protectedLength, aadRanges, aadLength};
}

const _parsersObuWithRtp: ReadonlyArray<Av1LayoutParser> = [
	computeLayoutFromSizeFieldObuStream,
	computeLayoutFromAnnexB,
	computeLayoutFromRtpPayload,
	computeLayoutFromRtxPayload,
];
const _parsersObuNoRtp: ReadonlyArray<Av1LayoutParser> = [
	computeLayoutFromSizeFieldObuStream,
	computeLayoutFromAnnexB,
	computeLayoutFromRtxPayload,
];
const _parsersRtpFirst: ReadonlyArray<Av1LayoutParser> = [
	computeLayoutFromRtpPayload,
	computeLayoutFromRtxPayload,
	computeLayoutFromSizeFieldObuStream,
	computeLayoutFromAnnexB,
];
const _parsersAll: ReadonlyArray<Av1LayoutParser> = [
	computeLayoutFromSizeFieldObuStream,
	computeLayoutFromAnnexB,
	computeLayoutFromRtpPayload,
	computeLayoutFromRtxPayload,
];

function validateAndWrapLayout(layout: Av1EncryptionLayoutBase, dataLength: number): Av1EncryptionLayout | undefined {
	for (let i = 0; i < layout.aadRanges.length; i++) {
		if (!isValidRange(layout.aadRanges[i], dataLength)) return undefined;
	}
	for (let i = 0; i < layout.protectedRanges.length; i++) {
		if (!isValidRange(layout.protectedRanges[i], dataLength)) return undefined;
	}
	return {
		protectedRanges: layout.protectedRanges,
		protectedLength: layout.protectedLength,
		aadRanges: layout.aadRanges,
		aadLength: layout.aadLength,
		buildAAD: (src) => buildByteArrayFromRanges(src, layout.aadRanges, layout.aadLength),
		extractProtected: (src) => buildByteArrayFromRanges(src, layout.protectedRanges, layout.protectedLength),
		writeProtected: (target, protectedBytes) =>
			writeByteArrayIntoRanges(target, layout.protectedRanges, protectedBytes, layout.protectedLength),
	};
}

export function computeAv1EncryptionLayout(data: Uint8Array): Av1EncryptionLayout | undefined {
	if (data.length === 0) return undefined;

	const firstByte = data[0];
	const looksLikeObuHeader = (firstByte & 0x80) === 0 && (firstByte & 0x01) === 0;
	const looksLikeRtpAggregationHeader = (firstByte & 0x07) === 0;

	let parsers: ReadonlyArray<Av1LayoutParser>;
	if (looksLikeObuHeader) {
		parsers = looksLikeRtpAggregationHeader ? _parsersObuWithRtp : _parsersObuNoRtp;
	} else if (looksLikeRtpAggregationHeader) {
		parsers = _parsersRtpFirst;
	} else {
		parsers = _parsersAll;
	}

	for (let i = 0; i < parsers.length; i++) {
		const layout = parsers[i](data);
		if (!layout) continue;

		const result = validateAndWrapLayout(layout, data.length);
		if (result) return result;
	}

	return undefined;
}
