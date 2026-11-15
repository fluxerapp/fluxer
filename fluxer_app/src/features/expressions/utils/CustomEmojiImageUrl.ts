// SPDX-License-Identifier: AGPL-3.0-or-later

import {setUrlQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';

export const CUSTOM_EMOJI_IMAGE_RUNG = 128;
export const CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG = 240;

const TEMPLATE_ID = '9999999999999999999';
const NUMERIC_ID = /^\d+$/;
const MAX_TEMPLATES = 64;
const templates = new Map<string, readonly [string, string] | null>();

function buildFromScratch(id: string, animated: boolean, size: number): string {
	const base = AvatarUtils.getEmojiURL({id, animated});
	if (!base) {
		return base;
	}
	return setUrlQueryParams(base, {size});
}

function templateFor(animated: boolean, size: number): readonly [string, string] | null {
	const base = AvatarUtils.getEmojiURL({id: TEMPLATE_ID, animated});
	if (!base) {
		return null;
	}
	const key = `${size}:${base}`;
	const known = templates.get(key);
	if (known !== undefined) {
		return known;
	}
	const parts = setUrlQueryParams(base, {size}).split(TEMPLATE_ID);
	const template = parts.length === 2 ? ([parts[0], parts[1]] as const) : null;
	if (templates.size >= MAX_TEMPLATES) {
		templates.clear();
	}
	templates.set(key, template);
	return template;
}

export function buildCustomEmojiURL({
	id,
	animated,
	size = CUSTOM_EMOJI_IMAGE_RUNG,
}: {
	id: string;
	animated: boolean;
	size?: number;
}): string {
	const template = isDesktopLocalAppDocument() && NUMERIC_ID.test(id) ? templateFor(animated, size) : null;
	if (template === null) {
		return buildFromScratch(id, animated, size);
	}
	return `${template[0]}${id}${template[1]}`;
}
