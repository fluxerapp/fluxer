// SPDX-License-Identifier: AGPL-3.0-or-later

export const BLUE_BGRA = [0xd9, 0x41, 0x46, 0xff];
export const WHITE_BGRA = [0xff, 0xff, 0xff, 0xff];

export function pixel(bitmap, width, x, y) {
	const offset = (y * width + x) * 4;
	return [...bitmap.subarray(offset, offset + 4)];
}

export function discBitmap(size) {
	const bitmap = Buffer.alloc(size * size * 4);
	const center = size / 2;
	for (let y = 0; y < size; y++) {
		for (let x = 0; x < size; x++) {
			const dx = x + 0.5 - center;
			const dy = y + 0.5 - center;
			if (dx * dx + dy * dy > center * center) continue;
			const color = Math.abs(dy) < size / 16 && Math.abs(dx) < size / 4 ? WHITE_BGRA : BLUE_BGRA;
			bitmap.set(color, (y * size + x) * 4);
		}
	}
	return bitmap;
}

export class FakeNativeImage {
	constructor() {
		this.representations = new Map();
		this.template = false;
	}

	static fromBitmap(size, scaleFactor = 1) {
		const image = new FakeNativeImage();
		const width = size * scaleFactor;
		image.addRepresentation({scaleFactor, width, height: width, buffer: discBitmap(width)});
		return image;
	}

	addRepresentation({scaleFactor = 1, width, height, buffer}) {
		this.representations.set(scaleFactor, {width, height, buffer: Buffer.from(buffer)});
	}

	isEmpty() {
		return this.representations.size === 0;
	}

	getScaleFactors() {
		return [...this.representations.keys()];
	}

	getSize(scaleFactor = 1) {
		const representation = this.representations.get(scaleFactor) ?? [...this.representations.values()][0];
		if (!representation) return {width: 0, height: 0};
		return {width: representation.width / scaleFactor, height: representation.height / scaleFactor};
	}

	toBitmap({scaleFactor = 1} = {}) {
		return Buffer.from(this.representations.get(scaleFactor).buffer);
	}

	resize() {
		return this;
	}

	setTemplateImage(template) {
		this.template = template;
	}

	isTemplateImage() {
		return this.template;
	}
}

export const fakeNativeImageModule = {
	createEmpty: () => new FakeNativeImage(),
	createFromPath: () => FakeNativeImage.fromBitmap(16),
};
