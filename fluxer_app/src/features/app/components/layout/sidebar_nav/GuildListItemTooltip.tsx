// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import type {SidebarVoiceRow} from '@app/features/app/hooks/useSidebarVoiceSummary';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import type {Guild} from '@app/features/guild/models/Guild';
import type {GuildCounts} from '@app/features/guild/state/GuildCount';
import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import {flxElementClassName} from '@app/lib/react';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {BellSlashIcon, MonitorPlayIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import type React from 'react';

const INVITES_PAUSED_RAID_TOOLTIP_DESCRIPTOR = msg({
	message: 'Invites are paused after a suspected raid',
	comment: 'Community sidebar tooltip shown while invites are paused after a suspected raid.',
});
const INVITES_PAUSED_MANUAL_TOOLTIP_DESCRIPTOR = msg({
	message: 'Invites are currently paused in this community',
	comment: 'Short label in the sidebar navigation guild list item.',
});
const ONLINE_MEMBER_COUNT_DESCRIPTOR = msg({
	message: '{onlineCount} online',
	comment: 'Community tooltip stat showing how many members are currently online.',
});
const ONLINE_COUNT_LOADING_LABEL_DESCRIPTOR = msg({
	message: 'online',
	comment: 'Community tooltip stat label shown next to a loading skeleton while the online count is loading.',
});
const TOTAL_MEMBER_COUNT_DESCRIPTOR = msg({
	message: '{memberCount} {rawMemberCount, plural, one {member} other {members}}',
	comment:
		'Community tooltip stat showing the total member count. memberCount is already localized for display; rawMemberCount controls plural grammar.',
});
const MEMBER_COUNT_LOADING_LABEL_DESCRIPTOR = msg({
	message: 'members',
	comment: 'Community tooltip stat label shown next to a loading skeleton while the member count is loading.',
});

const ONLINE_COUNT_SKELETON_WIDTH = '2.125rem';
const MEMBER_COUNT_SKELETON_WIDTH = '2.5rem';
const COUNT_SKELETON_HEIGHT = '0.6875rem';

interface GuildListItemTooltipProps {
	readonly guild: Guild;
	readonly canManageGuild: boolean;
	readonly isMuted: boolean;
	readonly mutedText: string | null;
	readonly guildCounts: GuildCounts | null;
	readonly currentLocale: string;
	readonly voiceRows: ReadonlyArray<SidebarVoiceRow>;
	readonly navigationKeybind: KeyCombo | null;
}

function VoiceRow({guildId, row}: {readonly guildId: string; readonly row: SidebarVoiceRow}) {
	let icon: React.ReactNode = (
		<SpeakerHighIcon
			className={styles.guildVoiceIcon}
			data-flx="app.sidebar-nav.guild-list-item-tooltip.voice-row.guild-voice-icon"
		/>
	);
	if (row.key === 'screenshare') {
		icon = (
			<MonitorPlayIcon
				weight="fill"
				className={styles.guildVoiceIcon}
				data-flx="app.sidebar-nav.guild-list-item-tooltip.voice-row.guild-voice-icon--2"
			/>
		);
	}
	return (
		<flx-app-guild-list-item-voice-info
			className={flxElementClassName(styles.guildVoiceInfo)}
			data-flx="app.sidebar-nav.guild-list-item-tooltip.voice-row.guild-voice-info"
		>
			{icon}
			<AvatarStack
				users={row.users}
				size={28}
				maxVisible={3}
				className={styles.guildVoiceAvatarStack}
				guildId={guildId}
				channelId={null}
				enableProfileModal={true}
				showTooltips={true}
				remainingContent={null}
				data-flx="app.sidebar-nav.guild-list-item-tooltip.voice-row.guild-voice-avatar-stack"
			/>
		</flx-app-guild-list-item-voice-info>
	);
}

export function GuildListItemTooltip({
	guild,
	canManageGuild,
	isMuted,
	mutedText,
	guildCounts,
	currentLocale,
	voiceRows,
	navigationKeybind,
}: GuildListItemTooltipProps) {
	const {i18n} = useLingui();
	const renderOnlineCount = (): React.ReactNode => {
		if (guildCounts == null) {
			return (
				<span
					className={styles.guildTooltipStatText}
					aria-hidden="true"
					data-flx="app.sidebar-nav.guild-list-item-tooltip.render-online-count.guild-tooltip-stat-text"
				>
					<SkeletonLine
						width={ONLINE_COUNT_SKELETON_WIDTH}
						height={COUNT_SKELETON_HEIGHT}
						data-flx="app.sidebar-nav.guild-list-item-tooltip.render-online-count.skeleton-line"
					/>
					<span data-flx="app.sidebar-nav.guild-list-item-tooltip.render-online-count.span">
						{i18n._(ONLINE_COUNT_LOADING_LABEL_DESCRIPTOR)}
					</span>
				</span>
			);
		}
		return (
			<span
				className={styles.guildTooltipStatText}
				data-flx="app.sidebar-nav.guild-list-item-tooltip.render-online-count.guild-tooltip-stat-text--2"
			>
				{i18n._(ONLINE_MEMBER_COUNT_DESCRIPTOR, {
					onlineCount: formatNumber(guildCounts.onlineCount, currentLocale),
				})}
			</span>
		);
	};
	const renderMemberCount = (): React.ReactNode => {
		if (guildCounts == null) {
			return (
				<span
					className={styles.guildTooltipStatText}
					aria-hidden="true"
					data-flx="app.sidebar-nav.guild-list-item-tooltip.render-member-count.guild-tooltip-stat-text"
				>
					<SkeletonLine
						width={MEMBER_COUNT_SKELETON_WIDTH}
						height={COUNT_SKELETON_HEIGHT}
						data-flx="app.sidebar-nav.guild-list-item-tooltip.render-member-count.skeleton-line"
					/>
					<span data-flx="app.sidebar-nav.guild-list-item-tooltip.render-member-count.span">
						{i18n._(MEMBER_COUNT_LOADING_LABEL_DESCRIPTOR)}
					</span>
				</span>
			);
		}
		return (
			<span
				className={styles.guildTooltipStatText}
				data-flx="app.sidebar-nav.guild-list-item-tooltip.render-member-count.guild-tooltip-stat-text--2"
			>
				{i18n._(TOTAL_MEMBER_COUNT_DESCRIPTOR, {
					memberCount: formatNumber(guildCounts.memberCount, currentLocale),
					rawMemberCount: guildCounts.memberCount,
				})}
			</span>
		);
	};
	let invitesPausedMessage: string | null = null;
	if (canManageGuild && guild.features.has(GuildFeatures.INVITES_DISABLED)) {
		if (guild.features.has(GuildFeatures.RAID_DETECTED)) {
			invitesPausedMessage = i18n._(INVITES_PAUSED_RAID_TOOLTIP_DESCRIPTOR);
		} else {
			invitesPausedMessage = i18n._(INVITES_PAUSED_MANUAL_TOOLTIP_DESCRIPTOR);
		}
	}
	return (
		<flx-app-guild-list-item-tooltip
			className={flxElementClassName(styles.guildTooltipContainer)}
			data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-container"
		>
			<flx-app-guild-list-item-tooltip-header
				className={flxElementClassName(styles.guildTooltipHeader)}
				data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-header"
			>
				<GuildBadge
					features={guild.features}
					showTooltip={false}
					onLightSurface
					data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-badge"
				/>
				<span className={styles.guildTooltipName} data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-name">
					{guild.name}
				</span>
			</flx-app-guild-list-item-tooltip-header>
			{guild.unavailable && (
				<span
					className={styles.guildTooltipMessage}
					data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-message"
				>
					<Trans>Something went wrong. We're working on it.</Trans>
				</span>
			)}
			{invitesPausedMessage != null && (
				<span
					className={styles.guildTooltipMessage}
					data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-message--2"
				>
					{invitesPausedMessage}
				</span>
			)}
			{isMuted && (
				<flx-app-guild-list-item-muted-info
					className={flxElementClassName(styles.guildMutedInfo)}
					data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-muted-info"
				>
					<BellSlashIcon
						weight="fill"
						className={styles.guildMutedIcon}
						data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-muted-icon"
					/>
					<span className={styles.guildMutedText} data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-muted-text">
						{mutedText}
					</span>
				</flx-app-guild-list-item-muted-info>
			)}
			<flx-app-guild-list-item-tooltip-stats
				className={flxElementClassName(styles.guildTooltipStats)}
				aria-busy={guildCounts == null}
				data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-stats"
			>
				<flx-app-guild-list-item-tooltip-stat
					className={flxElementClassName(styles.guildTooltipStat)}
					data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-stat"
				>
					<flx-app-guild-list-item-stat-dot
						className={flxElementClassName(styles.guildTooltipStatDot, styles.guildTooltipStatDotOnline)}
						data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-stat-dot"
					/>
					{renderOnlineCount()}
				</flx-app-guild-list-item-tooltip-stat>
				<flx-app-guild-list-item-tooltip-stat
					className={flxElementClassName(styles.guildTooltipStat)}
					data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-stat--2"
				>
					<flx-app-guild-list-item-stat-dot
						className={flxElementClassName(styles.guildTooltipStatDot, styles.guildTooltipStatDotMembers)}
						data-flx="app.sidebar-nav.guild-list-item-tooltip.guild-tooltip-stat-dot--2"
					/>
					{renderMemberCount()}
				</flx-app-guild-list-item-tooltip-stat>
			</flx-app-guild-list-item-tooltip-stats>
			{voiceRows.map((row) => (
				<VoiceRow
					key={row.key}
					guildId={guild.id}
					row={row}
					data-flx="app.sidebar-nav.guild-list-item-tooltip.voice-row"
				/>
			))}
			{navigationKeybind != null && (
				<KeybindHint combo={navigationKeybind} data-flx="app.sidebar-nav.guild-list-item-tooltip.keybind-hint" />
			)}
		</flx-app-guild-list-item-tooltip>
	);
}
