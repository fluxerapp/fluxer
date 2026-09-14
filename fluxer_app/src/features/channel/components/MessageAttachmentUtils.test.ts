// SPDX-License-Identifier: AGPL-3.0-or-later

import {splitMediaAndFileAttachments} from '@app/features/channel/components/MessageAttachmentUtils';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

const baseAttachment = {
	id: 'attachment-1',
	filename: 'file.bin',
	size: 1,
	url: 'https://example.com/file.bin',
	proxy_url: 'https://example.com/file.bin',
	flags: 0,
} satisfies MessageAttachment;

function attachment(overrides: Partial<MessageAttachment>): MessageAttachment {
	return {
		...baseAttachment,
		...overrides,
	};
}

describe('Message attachment utils', () => {
	it('does not put audio attachments in the mosaic media bucket', () => {
		const audio = attachment({
			id: 'audio-1',
			filename: 'same-channel-message.mp3',
			content_type: 'audio/mpeg',
			width: 0,
			height: 0,
			duration: 1,
		});
		const {mediaAttachments, fileAttachments} = splitMediaAndFileAttachments([audio]);
		expect(mediaAttachments).toEqual([]);
		expect(fileAttachments).toEqual([audio]);
	});
	it('puts positive-dimension visual attachments in the mosaic media bucket', () => {
		const image = attachment({
			id: 'image-1',
			filename: 'image.png',
			content_type: 'image/png',
			width: 640,
			height: 480,
		});
		const video = attachment({
			id: 'video-1',
			filename: 'video.mp4',
			content_type: 'video/mp4',
			width: 1280,
			height: 720,
		});
		const {mediaAttachments, fileAttachments} = splitMediaAndFileAttachments([image, video]);
		expect(mediaAttachments).toEqual([image, video]);
		expect(fileAttachments).toEqual([]);
	});
	it('does not put zero-dimension visual attachments in the mosaic media bucket', () => {
		const image = attachment({
			id: 'image-1',
			filename: 'image.png',
			content_type: 'image/png',
			width: 0,
			height: 0,
		});
		const {mediaAttachments, fileAttachments} = splitMediaAndFileAttachments([image]);
		expect(mediaAttachments).toEqual([]);
		expect(fileAttachments).toEqual([image]);
	});
});
