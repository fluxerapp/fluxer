// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DiscoveryGuild} from '@app/features/discovery/commands/DiscoveryCommands';
import {DiscoveryGuildPreviewModal} from '@app/features/discovery/components/modals/DiscoveryGuildPreviewModal';
import styles from '@app/features/discovery/discovery/DiscoveryGuildCard.module.css';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import {DiscoveryGuildContextMenu} from '@app/features/ui/action_menu/DiscoveryGuildContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useId, useMemo} from 'react';

const NO_DESCRIPTION_PROVIDED_DESCRIPTOR = msg({
	message: 'No description.',
	comment: 'Empty-state text in the discovery guild card.',
});
const ONLINE_DESCRIPTOR = msg({
	message: '{onlineCount} online',
	comment: 'Short label in the discovery guild card. Keep it concise. Preserve {onlineCount}; it is inserted by code.',
});
const PREVIEW_NAMED_COMMUNITY_DESCRIPTOR = msg({
	message: 'Preview {communityName}',
	comment:
		'Accessible label for a Discovery community card, which opens a preview of the community. {communityName} is the public community name.',
});

const DISCOVERY_NAME_BADGE_IGNORED_FEATURES = new Set<string>([GuildFeatures.DISCOVERABLE]);
const CARD_ICON_SIZE_PX = 56;

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

function resolveBannerTintClassName(guildId: string): string {
	const tintCount = BigInt(BANNER_TINT_CLASS_NAMES.length);
	return BANNER_TINT_CLASS_NAMES[Number(BigInt(guildId) % tintCount)];
}

interface DiscoveryGuildCardProps {
	guild: DiscoveryGuild;
	positionInSet: number;
	setSize: number;
}

export const DiscoveryGuildCard = observer(function DiscoveryGuildCard({
	guild,
	positionInSet,
	setSize,
}: DiscoveryGuildCardProps) {
	const {i18n} = useLingui();
	const baseId = useId();
	const nameId = `${baseId}-name`;
	const descriptionId = `${baseId}-description`;
	const statsId = `${baseId}-stats`;
	const onlineCount = formatNumber(guild.online_count, getCurrentLocale());
	const nameBadgeFeatures = guild.features.filter((feature) => !DISCOVERY_NAME_BADGE_IGNORED_FEATURES.has(feature));
	const bannerImageUrl = useMemo(() => {
		if (!guild.banner) {
			return null;
		}
		return AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}) || null;
	}, [guild.banner, guild.id]);
	const handleActivate = useCallback(() => {
		ModalCommands.pushWithKey(
			modal(() => (
				<DiscoveryGuildPreviewModal
					guild={guild}
					data-flx="discovery.discovery.discovery-guild-card.handle-activate.discovery-guild-preview-modal"
				/>
			)),
			`discovery-guild-preview-${guild.id}`,
		);
	}, [guild]);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, (props) => (
				<DiscoveryGuildContextMenu
					guild={{id: guild.id, name: guild.name}}
					onClose={props.onClose}
					data-flx="discovery.discovery.discovery-guild-card.handle-context-menu.discovery-guild-context-menu"
				/>
			));
		},
		[guild.id, guild.name],
	);
	return (
		<article
			role="listitem"
			aria-labelledby={nameId}
			aria-posinset={positionInSet}
			aria-setsize={setSize}
			className={styles.card}
			onContextMenu={handleContextMenu}
			data-flx="discovery.discovery.discovery-guild-card.card.context-menu"
		>
			<FocusRing offset={-2} data-flx="discovery.discovery.discovery-guild-card.focus-ring">
				<button
					type="button"
					className={styles.cardButton}
					onClick={handleActivate}
					aria-label={i18n._(PREVIEW_NAMED_COMMUNITY_DESCRIPTOR, {communityName: guild.name})}
					aria-describedby={`${descriptionId} ${statsId}`}
					data-flx="discovery.discovery.discovery-guild-card.card-button.activate"
				>
					<span className={styles.banner} aria-hidden data-flx="discovery.discovery.discovery-guild-card.banner">
						{bannerImageUrl ? (
							<>
								<span
									className={styles.bannerImage}
									style={{backgroundImage: `url(${bannerImageUrl})`}}
									data-flx="discovery.discovery.discovery-guild-card.banner-image"
								/>
								<span className={styles.bannerScrim} data-flx="discovery.discovery.discovery-guild-card.banner-scrim" />
							</>
						) : (
							<span
								className={resolveBannerTintClassName(guild.id)}
								data-flx="discovery.discovery.discovery-guild-card.span"
							/>
						)}
					</span>
					<span className={styles.body} data-flx="discovery.discovery.discovery-guild-card.body">
						<span className={styles.titleRow} data-flx="discovery.discovery.discovery-guild-card.title-row">
							<span
								id={nameId}
								className={styles.nameText}
								data-flx="discovery.discovery.discovery-guild-card.name-text"
							>
								{guild.name}
							</span>
							<GuildBadge
								features={nameBadgeFeatures}
								tooltipPosition="bottom"
								data-flx="discovery.discovery.discovery-guild-card.guild-badge"
							/>
						</span>
						<span
							id={descriptionId}
							className={styles.description}
							data-flx="discovery.discovery.discovery-guild-card.description"
						>
							{guild.description || i18n._(NO_DESCRIPTION_PROVIDED_DESCRIPTOR)}
						</span>
					</span>
					<span id={statsId} className={styles.footer} data-flx="discovery.discovery.discovery-guild-card.footer">
						<span className={styles.stat} data-flx="discovery.discovery.discovery-guild-card.stat">
							<span
								className={styles.statDotOnline}
								aria-hidden
								data-flx="discovery.discovery.discovery-guild-card.stat-dot-online"
							/>
							<span className={styles.statText} data-flx="discovery.discovery.discovery-guild-card.stat-text">
								{i18n._(ONLINE_DESCRIPTOR, {onlineCount})}
							</span>
						</span>
						<span className={styles.stat} data-flx="discovery.discovery.discovery-guild-card.stat--2">
							<span
								className={styles.statDotMembers}
								aria-hidden
								data-flx="discovery.discovery.discovery-guild-card.stat-dot-members"
							/>
							<span className={styles.statText} data-flx="discovery.discovery.discovery-guild-card.stat-text--2">
								<Trans>
									<Plural
										value={guild.member_count}
										one="# member"
										other="# members"
										data-flx="discovery.discovery.discovery-guild-card.plural"
									/>
								</Trans>
							</span>
						</span>
					</span>
				</button>
			</FocusRing>
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				sizePx={CARD_ICON_SIZE_PX}
				className={styles.icon}
				containerProps={{'aria-hidden': true}}
				data-flx="discovery.discovery.discovery-guild-card.icon"
			/>
		</article>
	);
});
