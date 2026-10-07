// SPDX-License-Identifier: AGPL-3.0-or-later

import avatar0 from '@app/media/images/avatars/0.png';
import avatar1 from '@app/media/images/avatars/1.png';
import avatar2 from '@app/media/images/avatars/2.png';
import avatar3 from '@app/media/images/avatars/3.png';
import avatar4 from '@app/media/images/avatars/4.png';
import avatar5 from '@app/media/images/avatars/5.png';

const DEFAULT_AVATARS: ReadonlyArray<string> = [avatar0, avatar1, avatar2, avatar3, avatar4, avatar5];

const DEFAULT_AVATAR_COUNT = DEFAULT_AVATARS.length;

export function getDefaultAvatarAssetURL(index: number): string {
	return DEFAULT_AVATARS[((index % DEFAULT_AVATAR_COUNT) + DEFAULT_AVATAR_COUNT) % DEFAULT_AVATAR_COUNT];
}
