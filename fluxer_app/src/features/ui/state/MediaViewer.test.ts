// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaViewer, {type MediaViewerItem} from '@app/features/ui/state/MediaViewer';
import {afterEach, describe, expect, it} from 'vitest';

const createItem = (overrides?: Partial<MediaViewerItem>): MediaViewerItem => ({
	src: 'https://cdn.example.test/media.png',
	originalSrc: 'https://example.test/media.png',
	naturalWidth: 640,
	naturalHeight: 480,
	type: 'image',
	...overrides,
});

describe('MediaViewer state', () => {
	afterEach(() => {
		MediaViewer.close();
	});
	it('copies and freezes media items when opening the viewer', () => {
		const item = createItem({filename: 'original.png'});
		const items = [item];
		MediaViewer.open(items, 0);
		items[0] = createItem({filename: 'mutated.png'});
		(item as {filename: string}).filename = 'changed-after-open.png';
		expect(MediaViewer.items).toHaveLength(1);
		expect(MediaViewer.items[0]).not.toBe(item);
		expect(MediaViewer.items[0].filename).toBe('original.png');
		expect(Object.isFrozen(MediaViewer.items)).toBe(true);
		expect(Object.isFrozen(MediaViewer.items[0])).toBe(true);
	});
});
