/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {ComparisonCheckRow} from '@app/components/modals/components/ComparisonCheckRow';
import {ComparisonRow} from '@app/components/modals/components/ComparisonRow';
import styles from '@app/components/modals/components/FeatureComparisonTable.module.css';
import {formatFileSize} from '@app/utils/FileUtils';
import {Limits} from '@app/utils/limits/UserLimits';
import {
	isBooleanPerk,
	isNumericPerk,
	isTextPerk,
	PLUTONIUM_PERKS,
	type PlutoniumPerk,
} from '@fluxer/constants/src/PlutoniumPerks';
import {formatNumber} from '@fluxer/number_utils/src/NumberFormatting';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

export const FeatureComparisonTable = observer(() => {
	const {t, i18n} = useLingui();
	const locale = i18n.locale;

	const perkLabels = useMemo(
		() => ({
			custom_4_digit_username_tag: t`Custom 4-digit username tag`,
			per_community_profiles: t`Per-community profiles`,
			message_scheduling: t`Message scheduling`,
			profile_badge: t`Profile badge`,
			custom_video_backgrounds: t`Custom video backgrounds`,
			entrance_sounds: t`Entrance sounds`,
			communities: t`Communities`,
			message_character_limit: t`Message character limit`,
			bookmarked_messages: t`Bookmarked messages`,
			file_upload_size: t`File upload size`,
			emoji_sticker_packs: t`Emoji & sticker packs`,
			saved_media: t`Saved media`,
			use_animated_emojis: t`Use animated emojis`,
			global_emoji_sticker_access: t`Global emoji & sticker access`,
			video_quality: t`Video quality`,
			animated_avatars_and_banners: t`Animated avatars & banners`,
			early_access: t`Early access to new features`,
			custom_themes: t`Custom themes`,
			video_quality_free: t`720p/30fps`,
			video_quality_premium: t`Up to 4K/60fps`,
		}),
		[t],
	);

	const availablePerks = useMemo(() => PLUTONIUM_PERKS.filter((perk) => perk.status === 'available'), []);

	const formatPerkValue = (perk: PlutoniumPerk, value: number, isPremium: boolean): string => {
		if (!isNumericPerk(perk)) return String(value);

		const resolvedValue = perk.limitKey
			? isPremium
				? Limits.getPremiumValue(perk.limitKey, value)
				: Limits.getFreeValue(perk.limitKey, value)
			: value;

		if (perk.unit === 'bytes') {
			return formatFileSize(resolvedValue);
		}
		return formatNumber(resolvedValue, locale);
	};

	const renderPerkRow = (perk: PlutoniumPerk) => {
		const label = perkLabels[perk.i18nKey as keyof typeof perkLabels] || perk.i18nKey;

		if (isBooleanPerk(perk)) {
			return (
				<ComparisonCheckRow key={perk.id} feature={label} freeHas={perk.freeValue} plutoniumHas={perk.plutoniumValue} />
			);
		}

		if (isNumericPerk(perk)) {
			return (
				<ComparisonRow
					key={perk.id}
					feature={label}
					freeValue={formatPerkValue(perk, perk.freeValue, false)}
					plutoniumValue={formatPerkValue(perk, perk.plutoniumValue, true)}
				/>
			);
		}

		if (isTextPerk(perk)) {
			const freeLabel = perkLabels[perk.freeValueI18nKey as keyof typeof perkLabels] || perk.freeValueI18nKey;
			const premiumLabel =
				perkLabels[perk.plutoniumValueI18nKey as keyof typeof perkLabels] || perk.plutoniumValueI18nKey;
			return <ComparisonRow key={perk.id} feature={label} freeValue={freeLabel} plutoniumValue={premiumLabel} />;
		}

		return null;
	};

	return (
		<div className={styles.table}>
			<div className={styles.header}>
				<div className={styles.headerFeature}>
					<p className={styles.headerFeatureText}>
						<Trans>Feature</Trans>
					</p>
				</div>
				<div className={styles.headerValues}>
					<div className={styles.headerFree}>
						<Trans>Free</Trans>
					</div>
					<div className={styles.headerPlutonium}>
						<Trans>Plutonium</Trans>
					</div>
				</div>
			</div>

			<div className={styles.rows}>{availablePerks.map(renderPerkRow)}</div>
		</div>
	);
});
