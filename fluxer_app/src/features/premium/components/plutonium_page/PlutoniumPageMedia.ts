// SPDX-License-Identifier: AGPL-3.0-or-later

import {hostedImageUrl} from '@app/features/app/config/HostedMedia';

export const CROWN_AVIF = hostedImageUrl('perks-plutonium-crown.4d0b72644330bf10.avif');
export const CROWN_WEBP = hostedImageUrl('perks-plutonium-crown.3265aa6b4147b19b.webp');

export const AVATAR_LSF = hostedImageUrl('perks-avatar-lsf.0800cc17b7d31b5e.webp');
export const BANNER_LSF = hostedImageUrl('perks-banner-lsf.05f2565afc72a167.webp');
export const EMOJI_CATVIBE = hostedImageUrl('perks-emoji-catvibe.28e83552cc9a26c2.webp');
export const EMOJI_CATWAVE = hostedImageUrl('perks-emoji-catwave.663abebab1c327fb.webp');
export const STICKER_FEEL_THAT = hostedImageUrl('perks-sticker-feel-that.3eae385d2d9cc99a.webp');

type ShotSet = [string, string, string];

export interface ShotFamily {
	avif: ShotSet;
	webp: ShotSet;
}

function hostedShotSet([small, medium, large]: ShotSet): ShotSet {
	return [hostedImageUrl(small), hostedImageUrl(medium), hostedImageUrl(large)];
}

function shotFamily(avif: ShotSet, webp: ShotSet): ShotFamily {
	return {avif: hostedShotSet(avif), webp: hostedShotSet(webp)};
}

export const EXPRESSIONS_SHOTS = shotFamily(
	[
		'screenshots-feature-perk-expressions-640w.481d53f29ebbe154.avif',
		'screenshots-feature-perk-expressions-1120w.609f056dd55a176f.avif',
		'screenshots-feature-perk-expressions-1600w.35a92d3fc43fa858.avif',
	],
	[
		'screenshots-feature-perk-expressions-640w.74bde0805e3452eb.webp',
		'screenshots-feature-perk-expressions-1120w.f52ad1f714185faa.webp',
		'screenshots-feature-perk-expressions-1600w.192c0d18c4a3d705.webp',
	],
);

export const PROFILE_SHOTS = shotFamily(
	[
		'screenshots-feature-perk-profile-640w.0df90fc828ae1bb4.avif',
		'screenshots-feature-perk-profile-1120w.ca5e8d03c4f7eac7.avif',
		'screenshots-feature-perk-profile-1600w.c790612e9d3295f7.avif',
	],
	[
		'screenshots-feature-perk-profile-640w.76efd2d804dc3c32.webp',
		'screenshots-feature-perk-profile-1120w.799d04fb886684b2.webp',
		'screenshots-feature-perk-profile-1600w.7459898617b3aa74.webp',
	],
);

export const STREAM_SHOTS = shotFamily(
	[
		'screenshots-feature-perk-stream-640w.d0567b22ca4d9910.avif',
		'screenshots-feature-perk-stream-1120w.07ea4f292b8953a5.avif',
		'screenshots-feature-perk-stream-1600w.c9697ce430b0773e.avif',
	],
	[
		'screenshots-feature-perk-stream-640w.2848435d90d0afa5.webp',
		'screenshots-feature-perk-stream-1120w.a2076637a71b99bd.webp',
		'screenshots-feature-perk-stream-1600w.745d8f0f60d30672.webp',
	],
);

export const UPLOAD_SHOTS = shotFamily(
	[
		'screenshots-feature-perk-upload-640w.455d1bdceab21b42.avif',
		'screenshots-feature-perk-upload-1120w.392656a9b0f21585.avif',
		'screenshots-feature-perk-upload-1600w.1aa744f6f0122c3f.avif',
	],
	[
		'screenshots-feature-perk-upload-640w.5b80c11e2a6dfe63.webp',
		'screenshots-feature-perk-upload-1120w.156d068948c832fb.webp',
		'screenshots-feature-perk-upload-1600w.bc3223b318081269.webp',
	],
);
