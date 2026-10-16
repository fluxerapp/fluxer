// SPDX-License-Identifier: AGPL-3.0-or-later

import {setUrlQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';

export const CUSTOM_EMOJI_IMAGE_RUNG = 128;
export const CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG = 240;

export function buildCustomEmojiURL({
	id,
	animated,
	size = CUSTOM_EMOJI_IMAGE_RUNG,
}: {
	id: string;
	animated: boolean;
	size?: number;
}): string {
	const base = AvatarUtils.getEmojiURL({id, animated});
	if (!base) {
		return base;
	}
	return setUrlQueryParams(base, {size});
}
