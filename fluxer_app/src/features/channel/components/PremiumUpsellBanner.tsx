// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import DismissedUpsell from '@app/features/premium/state/DismissedUpsell';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {PlutoniumUpsell} from '@app/features/ui/plutonium_upsell/PlutoniumUpsell';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface PremiumUpsellBannerProps {
	children?: React.ReactNode;
	message?: React.ReactNode;
	communityIds?: Array<string>;
	communityCount?: number;
	previewContent?: React.ReactNode;
}

const COMMUNITY_ICON_LIMIT = 4;
const PREMIUM_EMOJI_STICKER_UPSELL_DESCRIPTOR = msg({
	message: 'Unlock all custom emojis and stickers across all communities with {premiumProductName}',
	comment: 'Premium upsell banner shown in emoji and sticker pickers.',
});
const mapGuildIdToIcon = (communityId: string) => {
	const guild = Guilds.getGuild(communityId);
	return (
		<GuildIcon
			key={communityId}
			id={communityId}
			name={guild?.name ?? ''}
			icon={guild?.icon ?? null}
			sizePx={20}
			className={styles.communityIcon}
			data-flx="channel.premium-upsell-banner.map-guild-id-to-icon.community-icon"
		/>
	);
};
export const PremiumUpsellBanner = observer(
	({children, message, communityIds, communityCount, previewContent}: PremiumUpsellBannerProps) => {
		const {i18n} = useLingui();
		if (DismissedUpsell.pickerPremiumUpsellDismissed) {
			return null;
		}
		const handleClick = () => {
			PremiumModalCommands.open();
		};
		const handleDismiss = () => {
			DismissedUpsell.dismissPickerPremiumUpsell();
		};
		const renderedCommunityIds = communityIds?.slice(0, COMMUNITY_ICON_LIMIT) ?? [];
		const extraCommunityCount =
			communityCount && communityCount > renderedCommunityIds.length ? communityCount - renderedCommunityIds.length : 0;
		return (
			<PlutoniumUpsell
				className={styles.banner}
				onButtonClick={handleClick}
				dismissible={true}
				onDismiss={handleDismiss}
				data-flx="channel.premium-upsell-banner.banner"
			>
				<div className={styles.content} data-flx="channel.premium-upsell-banner.content">
					<p className={styles.text} data-flx="channel.premium-upsell-banner.text">
						{message ??
							children ??
							i18n._(PREMIUM_EMOJI_STICKER_UPSELL_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
					</p>
					{renderedCommunityIds.length > 0 && (
						<div className={styles.communityRow} data-flx="channel.premium-upsell-banner.community-row">
							{renderedCommunityIds.map(mapGuildIdToIcon)}
							{extraCommunityCount > 0 && (
								<span className={styles.communityMore} data-flx="channel.premium-upsell-banner.community-more">
									+{extraCommunityCount}
								</span>
							)}
						</div>
					)}
					{previewContent && (
						<div className={styles.previewRow} data-flx="channel.premium-upsell-banner.preview-row">
							{previewContent}
						</div>
					)}
				</div>
			</PlutoniumUpsell>
		);
	},
);
