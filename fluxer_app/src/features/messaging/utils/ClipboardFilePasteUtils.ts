// SPDX-License-Identifier: AGPL-3.0-or-later

type ClipboardItemLike = {
	readonly types?: ReadonlyArray<string>;
	getType?: (type: string) => Promise<Blob>;
};

type AsyncClipboardLike = {
	read?: () => Promise<ReadonlyArray<ClipboardItemLike>>;
};

const IMAGE_EXTENSION_BY_MIME_TYPE = new Map([
	['image/png', '.png'],
	['image/jpeg', '.jpg'],
	['image/jpg', '.jpg'],
	['image/gif', '.gif'],
	['image/webp', '.webp'],
	['image/avif', '.avif'],
	['image/svg+xml', '.svg'],
]);

function extensionForImageType(type: string): string {
	return IMAGE_EXTENSION_BY_MIME_TYPE.get(type.toLowerCase()) ?? '';
}

function isUsableClipboardFile(file: File): boolean {
	return file.size > 0 || file.type.length > 0 || file.name.length > 0;
}

export function getClipboardDataFiles(clipboardData: DataTransfer | null | undefined): Array<File> {
	if (!clipboardData?.files) {
		return [];
	}
	return Array.from(clipboardData.files).filter(isUsableClipboardFile);
}

function createClipboardImageFile(blob: Blob, index: number, preferredType: string): File {
	if (blob instanceof File && blob.name) {
		return blob;
	}
	const type = blob.type || preferredType || 'application/octet-stream';
	const extension = extensionForImageType(type);
	return new File([blob], `clipboard-image-${index + 1}${extension}`, {
		type,
		lastModified: Date.now(),
	});
}

export async function readClipboardImageFiles(
	clipboard: AsyncClipboardLike | null | undefined = navigator.clipboard,
): Promise<Array<File>> {
	if (typeof clipboard?.read !== 'function') {
		return [];
	}
	let items: ReadonlyArray<ClipboardItemLike>;
	try {
		items = await clipboard.read();
	} catch {
		return [];
	}
	const files: Array<File> = [];
	for (const item of items) {
		const imageTypes = item.types?.filter((type) => type.toLowerCase().startsWith('image/')) ?? [];
		if (imageTypes.length === 0 || typeof item.getType !== 'function') {
			continue;
		}
		for (const imageType of imageTypes) {
			try {
				const blob = await item.getType(imageType);
				if (blob.size === 0) {
					continue;
				}
				files.push(createClipboardImageFile(blob, files.length, imageType));
			} catch {}
		}
	}
	return files;
}
