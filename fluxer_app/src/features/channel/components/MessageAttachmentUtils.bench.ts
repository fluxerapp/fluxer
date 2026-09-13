// SPDX-License-Identifier: AGPL-3.0-or-later

import {splitMediaAndFileAttachments} from '@app/features/channel/components/MessageAttachmentUtils';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {bench, describe} from 'vitest';

const CONTENT_TYPES = [
	'image/png',
	'image/jpeg',
	'video/mp4',
	'application/pdf',
	'audio/mpeg',
	'image/webp',
	'text/plain',
] as const;

const ATTACHMENTS = Array.from({length: 500}, (_value, index): MessageAttachment => {
	const contentType = CONTENT_TYPES[index % CONTENT_TYPES.length];
	const isVisual = contentType.startsWith('image/') || contentType.startsWith('video/');
	return {
		id: `attachment-${index}`,
		filename: `file-${index}`,
		size: 1024 + index,
		url: `https://cdn.example.test/${index}`,
		proxy_url: `https://cdn.example.test/${index}`,
		content_type: contentType,
		width: isVisual ? 640 + (index % 100) : 0,
		height: isVisual ? 360 + (index % 100) : 0,
		flags: 0,
	};
});

describe('MessageAttachmentUtils benchmarks', () => {
	bench('split 500 mixed media and file attachments', () => {
		splitMediaAndFileAttachments(ATTACHMENTS);
	});
});
