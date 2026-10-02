// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/discovery/utils/DiscoveryBannerTint.module.css';

const BANNER_TINT_CLASS_NAMES = [
	styles.bannerTintViolet,
	styles.bannerTintBlue,
	styles.bannerTintTeal,
	styles.bannerTintGreen,
	styles.bannerTintLime,
	styles.bannerTintAmber,
	styles.bannerTintCoral,
	styles.bannerTintRose,
	styles.bannerTintMagenta,
	styles.bannerTintIndigo,
] as const;

export function resolveBannerTintClassName(guildId: string): string {
	const tintCount = BigInt(BANNER_TINT_CLASS_NAMES.length);
	return BANNER_TINT_CLASS_NAMES[Number(BigInt(guildId) % tintCount)];
}
