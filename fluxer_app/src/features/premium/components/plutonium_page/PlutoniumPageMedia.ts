// SPDX-License-Identifier: AGPL-3.0-or-later

import {hostedImageUrl} from '@app/features/app/config/HostedMedia';
import plutoniumAtomAvif from '@app/media/images/premium/plutonium-atom.avif';
import plutoniumAtomWebp from '@app/media/images/premium/plutonium-atom.webp';
import plutoniumAtomAvif2x from '@app/media/images/premium/plutonium-atom-960w.avif';
import plutoniumAtomWebp2x from '@app/media/images/premium/plutonium-atom-960w.webp';
import premiumCrownAvif from '@app/media/images/premium/premium-crown.avif';
import premiumCrownWebp from '@app/media/images/premium/premium-crown.webp';
import premiumCrownAvif2x from '@app/media/images/premium/premium-crown-960w.avif';
import premiumCrownWebp2x from '@app/media/images/premium/premium-crown-960w.webp';

export interface HeroArt {
	avifSrcSet: string;
	webpSrcSet: string;
	src: string;
	width: number;
	height: number;
}

function densitySrcSet(oneX: string, twoX: string): string {
	return `${oneX} 1x, ${twoX} 2x`;
}

const PLUTONIUM_ATOM_ART: HeroArt = {
	avifSrcSet: densitySrcSet(plutoniumAtomAvif, plutoniumAtomAvif2x),
	webpSrcSet: densitySrcSet(plutoniumAtomWebp, plutoniumAtomWebp2x),
	src: plutoniumAtomWebp,
	width: 480,
	height: 524,
};

const PREMIUM_CROWN_ART: HeroArt = {
	avifSrcSet: densitySrcSet(premiumCrownAvif, premiumCrownAvif2x),
	webpSrcSet: densitySrcSet(premiumCrownWebp, premiumCrownWebp2x),
	src: premiumCrownWebp,
	width: 480,
	height: 415,
};

export function heroArtFor(selfHosted: boolean): HeroArt {
	return selfHosted ? PREMIUM_CROWN_ART : PLUTONIUM_ATOM_ART;
}

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
		'screenshots-feature-perk-profile-640w.2c3a1ece3b5c8f37.avif',
		'screenshots-feature-perk-profile-1120w.d4b166dad6f99aaa.avif',
		'screenshots-feature-perk-profile-1600w.c5097ad361bc1326.avif',
	],
	[
		'screenshots-feature-perk-profile-640w.46700ee4bed25529.webp',
		'screenshots-feature-perk-profile-1120w.7b4b789270977507.webp',
		'screenshots-feature-perk-profile-1600w.7b2d691cea124f1b.webp',
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
