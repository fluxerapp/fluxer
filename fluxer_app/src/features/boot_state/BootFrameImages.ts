// SPDX-License-Identifier: AGPL-3.0-or-later

import {BOOT_FRAME_IMAGE_SOURCE_ATTRIBUTE} from '@app/features/boot_state/BootStateBrowserFormat';

const MAX_IMAGE_BYTES = 384 * 1024;
const MAX_IMAGE_EDGE = 512;
const CAPTURE_FETCH_BUDGET_MS = 150;

interface ImageLayout {
	readonly width: number;
	readonly height: number;
	readonly naturalWidth: number;
	readonly naturalHeight: number;
	readonly scale: number;
	readonly fit: string;
	readonly position: string;
}

function rasterize(source: CanvasImageSource, layout: ImageLayout): string | null {
	const {width, height, naturalWidth, naturalHeight, fit, position} = layout;
	const canvas = document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	try {
		const context = canvas.getContext('2d');
		if (context === null) return null;
		let drawWidth = width;
		let drawHeight = height;
		if (fit !== 'fill') {
			let scale = Math.min(width / naturalWidth, height / naturalHeight);
			if (fit === 'cover') scale = Math.max(width / naturalWidth, height / naturalHeight);
			if (fit === 'none') scale = layout.scale;
			if (fit === 'scale-down') scale = Math.min(scale, layout.scale);
			drawWidth = naturalWidth * scale;
			drawHeight = naturalHeight * scale;
		}
		const [x, y] = position.split(' ');
		const offset = (value: string, space: number) =>
			value.endsWith('%') ? (space * Number.parseFloat(value)) / 100 : Number.parseFloat(value) * layout.scale;
		context.drawImage(source, offset(x, width - drawWidth), offset(y, height - drawHeight), drawWidth, drawHeight);
		const webp = canvas.toDataURL('image/webp', 1);
		return webp.startsWith('data:image/webp;') ? webp : canvas.toDataURL('image/png');
	} catch {
		return null;
	} finally {
		canvas.width = 0;
		canvas.height = 0;
	}
}

export async function captureFrameImages(
	source: HTMLElement,
	clone: HTMLElement,
	prepare: () => void,
): Promise<Array<string> | null> {
	const originals = Array.from(source.querySelectorAll('img'), (image, index) => {
		const rect = image.getBoundingClientRect();
		const visible =
			rect.bottom > 0 &&
			rect.top < innerHeight &&
			rect.right > 0 &&
			rect.left < innerWidth &&
			rect.width > 0 &&
			rect.height > 0;
		return {image, index, rect, visible};
	}).sort((left, right) => Number(right.visible) - Number(left.visible));
	const copies = Array.from(clone.querySelectorAll('img'));
	prepare();
	const retained = originals.filter(({index, visible}) => {
		const copy = copies[index];
		return copy !== undefined && clone.contains(copy) && (visible || copy.closest('[data-flx-message-id]') !== null);
	});
	const warmed = new Set<string>();
	const controller = new AbortController();
	const timer = setTimeout(() => controller.abort(), CAPTURE_FETCH_BUDGET_MS);
	const captured = new Map<string, Promise<string | null>>();
	let bytes = 0;
	try {
		for (const visible of [true, false])
			await Promise.all(
				retained
					.filter((entry) => entry.visible === visible)
					.map(async ({image, index, rect, visible}) => {
						const copy = copies[index];
						if (!copy || !image.complete || image.naturalWidth === 0 || rect.width <= 0 || rect.height <= 0) return;
						const width = Math.min(MAX_IMAGE_EDGE, Math.ceil(rect.width * devicePixelRatio));
						const height = Math.min(MAX_IMAGE_EDGE, Math.ceil(rect.height * devicePixelRatio));
						const style = getComputedStyle(image);
						const layout: ImageLayout = {
							width,
							height,
							naturalWidth: image.naturalWidth,
							naturalHeight: image.naturalHeight,
							scale: width / rect.width,
							fit: style.objectFit,
							position: style.objectPosition,
						};
						const original = image.getAttribute('src');
						if (!original) return;
						const cacheKey = `${image.currentSrc}|${width}|${height}|${layout.fit}|${layout.position}`;
						let pending = captured.get(cacheKey);
						if (!pending) {
							pending = (async () => {
								const direct = rasterize(image, layout);
								if (direct !== null) return direct;
								try {
									const response = await fetch(image.currentSrc, {
										mode: 'cors',
										cache: 'force-cache',
										signal: controller.signal,
									});
									if (!response.ok) return null;
									const bitmap = await createImageBitmap(await response.blob());
									try {
										return rasterize(bitmap, layout);
									} finally {
										bitmap.close();
									}
								} catch {
									return null;
								}
							})();
							captured.set(cacheKey, pending);
						}
						const data = await pending;
						if (data === null) return;
						const cost = data.length + (visible && !warmed.has(data) ? data.length : 0);
						if (bytes + cost > MAX_IMAGE_BYTES) return;
						bytes += cost;
						if (visible) warmed.add(data);
						copy.removeAttribute('srcset');
						copy.removeAttribute('sizes');
						copy.setAttribute('src', data);
						copy.setAttribute('loading', 'eager');
						if (!original.startsWith('data:')) copy.setAttribute(BOOT_FRAME_IMAGE_SOURCE_ATTRIBUTE, original);
					}),
			);
	} finally {
		clearTimeout(timer);
		controller.abort();
	}
	const visibleSources = new Set<string>();
	for (const {index, visible} of retained) {
		const source = copies[index]?.getAttribute('src');
		if (visible && source) visibleSources.add(source);
	}
	const embedded = Array.from(clone.querySelectorAll('img[src]'), (image) => image.getAttribute('src')!);
	embedded.push(...visibleSources);
	const total = embedded.reduce((bytes, source) => bytes + (source.startsWith('data:') ? source.length : 0), 0);
	return total <= MAX_IMAGE_BYTES ? Array.from(visibleSources) : null;
}

export function retainFrameImages(frame: HTMLElement, live: HTMLElement, signal: AbortSignal): void {
	if (signal.aborted) return;
	const sources = new Map<string, {data: string; area: number}>();
	for (const image of frame.querySelectorAll<HTMLImageElement>(`img[${BOOT_FRAME_IMAGE_SOURCE_ATTRIBUTE}]`)) {
		const original = image.getAttribute(BOOT_FRAME_IMAGE_SOURCE_ATTRIBUTE);
		const data = image.getAttribute('src');
		const rect = image.getBoundingClientRect();
		const area = rect.width * rect.height;
		if (original !== null && data !== null && area >= (sources.get(original)?.area ?? 0))
			sources.set(original, {data, area});
	}
	const pending = new Map<HTMLImageElement, {data: string; restore: () => void; stop: () => void}>();
	const finish = () => {
		if (pending.size > 0) return;
		clearTimeout(timer);
		observer.disconnect();
		signal.removeEventListener('abort', cancel);
	};
	const cancel = () => {
		for (const lease of pending.values()) {
			lease.restore();
			lease.stop();
		}
	};
	const observer = new MutationObserver(() => {
		for (const [image, lease] of pending) {
			if (!image.isConnected || image.getAttribute('src') !== lease.data) lease.stop();
		}
	});
	const timer = setTimeout(() => {
		for (const lease of pending.values()) lease.stop();
	}, 10_000);
	observer.observe(live, {childList: true, subtree: true, attributes: true, attributeFilter: ['src']});
	signal.addEventListener('abort', cancel, {once: true});
	for (const image of live.querySelectorAll('img')) {
		const original = image.getAttribute('src');
		const snapshot = original === null ? undefined : sources.get(original);
		if (snapshot === undefined || (image.complete && image.naturalWidth > 0)) continue;
		const {data} = snapshot;
		const srcset = image.getAttribute('srcset');
		const sizes = image.getAttribute('sizes');
		const loading = new Image();
		const lease = {
			data,
			restore: () => {
				if (image.getAttribute('src') !== data) return;
				image.setAttribute('src', original!);
				if (srcset !== null) image.setAttribute('srcset', srcset);
			},
			stop: () => {
				pending.delete(image);
				loading.removeAttribute('src');
				loading.removeAttribute('srcset');
				finish();
			},
		};
		pending.set(image, lease);
		image.removeAttribute('srcset');
		image.src = data;
		if (srcset !== null) loading.srcset = srcset;
		if (sizes !== null) loading.sizes = sizes;
		loading.src = original!;
		void loading
			.decode()
			.then(
				() => {
					if (pending.get(image) === lease && !signal.aborted && image.isConnected) lease.restore();
				},
				() => undefined,
			)
			.finally(lease.stop);
	}
	finish();
}
