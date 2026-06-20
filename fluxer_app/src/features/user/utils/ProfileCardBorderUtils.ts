// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAccentColorHex, getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import {dimColor} from '@app/features/theme/utils/ColorUtils';
import type {User} from '@app/features/user/models/User';

type RawProfileColor = number | null | undefined;

export interface ProfileCardBorderPresentation {
	borderColor: string;
	borderGradient: string;
}

function buildProfileCardBorderGradient(sourceHex: string): string {
	const soft = dimColor(sourceHex, 0.22);
	const mid = dimColor(sourceHex, 0.08);
	return `linear-gradient(135deg, ${soft} 0%, ${sourceHex} 40%, ${mid} 72%, ${soft} 100%)`;
}

function resolveBorderSourceHex(
	user: User | null | undefined,
	accentColor: RawProfileColor,
	bannerColor: RawProfileColor,
	hasBannerImage: boolean,
): string {
	const accent = getUserAccentColor(user, accentColor);
	const bannerHex = getAccentColorHex(bannerColor);
	if (hasBannerImage && bannerHex) return bannerHex;
	return accent;
}

export function getProfileCardBannerFallbackColor(
	user: User | null | undefined,
	accentColor?: RawProfileColor,
	bannerColor?: RawProfileColor,
	hasBannerImage = false,
): string {
	return resolveBorderSourceHex(user, accentColor, bannerColor, hasBannerImage);
}

export function getProfileCardBorderPresentation(
	user: User | null | undefined,
	accentColor?: RawProfileColor,
	bannerColor?: RawProfileColor,
	hasBannerImage = false,
): ProfileCardBorderPresentation {
	const sourceHex = resolveBorderSourceHex(user, accentColor, bannerColor, hasBannerImage);
	return {
		borderColor: sourceHex,
		borderGradient: buildProfileCardBorderGradient(sourceHex),
	};
}
