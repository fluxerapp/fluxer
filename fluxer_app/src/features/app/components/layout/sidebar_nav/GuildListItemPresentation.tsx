// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {LongPressable} from '@app/features/app/components/LongPressable';
import type {GuildItemReorderIndicator} from '@app/features/app/components/layout/dnd/GuildReorderStateMachine';
import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import type {GuildListIndicatorBarTarget} from '@app/features/app/components/layout/sidebar_nav/GuildListIndicator';
import type {GuildListItemDragAndDrop} from '@app/features/app/components/layout/sidebar_nav/UseGuildListItemDragAndDrop';
import {VoiceBadge, type VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import {DropPlacement} from '@app/features/app/components/layout/types/DndTypes';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {
	type GuildInitialsLength,
	getGuildIconDisplayInitials,
	getInitialsFromName,
	getInitialsLength,
	truncateInitials,
} from '@app/features/guild/utils/GuildInitialsUtils';
import {ariaCurrentPage} from '@app/features/platform/ARIAProps';
import {Edge} from '@app/features/ui/AxisOrientation';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import {AnimePresence, AnimeSpan, type AnimeStyle, createAnimeFlxElement} from '@app/features/ui/motion/AnimeElement';
import * as AvatarSourceUtils from '@app/features/user/utils/AvatarSourceUtils';
import {flxElementClassName} from '@app/lib/react';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {ExclamationMarkIcon, PauseIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {forwardRef, useMemo} from 'react';
import invariant from 'tiny-invariant';

const GuildListItemIcon = createAnimeFlxElement('flx-app-guild-list-item-icon');

interface GuildListItemMotion {
	readonly iconTweenDuration: number;
	readonly indicatorLeave: Readonly<{opacity: number}>;
	readonly indicatorTweenDuration: number;
}

const REDUCED_GUILD_LIST_ITEM_MOTION: GuildListItemMotion = Object.freeze({
	iconTweenDuration: 0,
	indicatorLeave: Object.freeze({opacity: 1}),
	indicatorTweenDuration: 0,
});
const STANDARD_GUILD_LIST_ITEM_MOTION: GuildListItemMotion = Object.freeze({
	iconTweenDuration: 0.07,
	indicatorLeave: Object.freeze({opacity: 0}),
	indicatorTweenDuration: 0.2,
});

function resolveGuildListItemMotion(useReducedMotion: boolean): GuildListItemMotion {
	if (useReducedMotion) {
		return REDUCED_GUILD_LIST_ITEM_MOTION;
	}
	return STANDARD_GUILD_LIST_ITEM_MOTION;
}

function resolveInitialsLength(rawInitials: string): GuildInitialsLength | null {
	if (rawInitials.length === 0) {
		return null;
	}
	return getInitialsLength(rawInitials);
}

function resolveDraggingCursor(dragAndDrop: GuildListItemDragAndDrop | null): 'grabbing' | null {
	if (dragAndDrop == null) {
		return null;
	}
	if (!dragAndDrop.isDragging) {
		return null;
	}
	return 'grabbing';
}

function resolveGuildIconStyle(backgroundImage: string | null, draggingCursor: 'grabbing' | null): AnimeStyle {
	const style: Record<string, string> = {};
	if (backgroundImage != null) {
		style.backgroundImage = backgroundImage;
	}
	if (draggingCursor != null) {
		style.cursor = draggingCursor;
	}
	return style;
}

interface GuildListItemBadgesProps {
	readonly guild: Guild;
	readonly mentionCount: number;
	readonly canManageGuild: boolean;
	readonly hasVoiceActivity: boolean;
	readonly voiceBadgeActivity: VoiceBadgeActivity | null;
}

function GuildListItemBadges({
	guild,
	mentionCount,
	canManageGuild,
	hasVoiceActivity,
	voiceBadgeActivity,
}: GuildListItemBadgesProps) {
	return (
		<>
			{!guild.unavailable && (
				<flx-app-guild-list-item-badge
					className={flxElementClassName(styles.guildBadge, mentionCount > 0 && styles.guildBadgeActive)}
					data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-badge"
				>
					<MentionBadgeAnimated
						mentionCount={mentionCount}
						size="small"
						data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.mention-badge-animated"
					/>
				</flx-app-guild-list-item-badge>
			)}
			{voiceBadgeActivity != null && (
				<VoiceBadge
					activity={voiceBadgeActivity}
					data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.voice-badge"
				/>
			)}
			{canManageGuild &&
				guild.features.has(GuildFeatures.INVITES_DISABLED) &&
				mentionCount === 0 &&
				!hasVoiceActivity && (
					<flx-app-guild-list-item-invites-badge
						className={flxElementClassName(styles.guildInvitesPausedBadge)}
						data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-invites-paused-badge"
					>
						<flx-app-guild-list-item-invites-icon
							className={flxElementClassName(styles.guildInvitesPausedBadgeInner)}
							data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-invites-paused-badge-inner"
						>
							<PauseIcon
								weight="fill"
								className={styles.guildInvitesPausedIcon}
								data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-invites-paused-icon"
							/>
						</flx-app-guild-list-item-invites-icon>
					</flx-app-guild-list-item-invites-badge>
				)}
			{guild.unavailable && (
				<flx-app-guild-list-item-error-badge
					className={flxElementClassName(styles.guildErrorBadge)}
					data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-error-badge"
				>
					<flx-app-guild-list-item-error-icon
						className={flxElementClassName(styles.guildErrorBadgeInner)}
						data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-error-badge-inner"
					>
						<ExclamationMarkIcon
							weight="regular"
							className={styles.guildErrorIcon}
							data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges.guild-error-icon"
						/>
					</flx-app-guild-list-item-error-icon>
				</flx-app-guild-list-item-error-badge>
			)}
		</>
	);
}

function CombinePreviewIcon({guild}: {readonly guild: Guild}) {
	const url = AvatarSourceUtils.getGuildIconURL(guild, false);
	const initials = truncateInitials(getInitialsFromName(guild.name), 2);
	const backgroundStyle: React.CSSProperties = {};
	if (url.length > 0) {
		backgroundStyle.backgroundImage = `url(${url})`;
	}
	return (
		<flx-app-combine-preview-icon
			className={flxElementClassName(styles.combinePreviewIcon, url.length === 0 && styles.combinePreviewIconInitials)}
			style={backgroundStyle}
			data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview-icon.combine-preview-icon"
		>
			{url.length === 0 && (
				<span data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview-icon.span">{initials}</span>
			)}
		</flx-app-combine-preview-icon>
	);
}

function CombinePreview({targetGuild, sourceGuildId}: {readonly targetGuild: Guild; readonly sourceGuildId: string}) {
	const sourceGuild = Guilds.getGuild(sourceGuildId);
	invariant(sourceGuild, `guild combine preview source guild missing for guild=${sourceGuildId}`);
	const iconGuilds = [targetGuild, sourceGuild];
	return (
		<flx-app-combine-preview
			className={flxElementClassName(styles.combinePreview)}
			data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview.combine-preview"
		>
			<flx-app-combine-preview-grid
				className={flxElementClassName(styles.combinePreviewGrid)}
				data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview.combine-preview-grid"
			>
				{iconGuilds.map((guild) => (
					<CombinePreviewIcon
						key={guild.id}
						guild={guild}
						data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview.combine-preview-icon"
					/>
				))}
				<flx-app-combine-preview-icon
					className={flxElementClassName(styles.combinePreviewIcon, styles.combinePreviewIconEmpty)}
					data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview.combine-preview-icon--2"
				/>
				<flx-app-combine-preview-icon
					className={flxElementClassName(styles.combinePreviewIcon, styles.combinePreviewIconEmpty)}
					data-flx="app.sidebar-nav.guild-list-item-presentation.combine-preview.combine-preview-icon--3"
				/>
			</flx-app-combine-preview-grid>
		</flx-app-combine-preview>
	);
}

function resolveDropIndicator(dragAndDrop: GuildListItemDragAndDrop | null): GuildItemReorderIndicator | null {
	if (dragAndDrop == null) {
		return null;
	}
	return dragAndDrop.dropIndicator;
}

function renderCombinePreview(
	targetGuild: Guild,
	dragAndDrop: GuildListItemDragAndDrop | null,
	dropIndicator: GuildItemReorderIndicator | null,
): React.ReactNode {
	if (dropIndicator !== DropPlacement.COMBINE) {
		return null;
	}
	if (dragAndDrop == null) {
		return null;
	}
	const sourceGuildId = dragAndDrop.combineSourceGuildId;
	if (sourceGuildId == null) {
		return null;
	}
	return (
		<CombinePreview
			targetGuild={targetGuild}
			sourceGuildId={sourceGuildId}
			data-flx="app.sidebar-nav.guild-list-item-presentation.render-combine-preview.combine-preview"
		/>
	);
}

export interface GuildListItemPresentationProps {
	readonly guild: Guild;
	readonly isSortingList: boolean;
	readonly isSelected: boolean;
	readonly contextMenuOpen: boolean;
	readonly showGuildIndicator: boolean;
	readonly indicatorTarget: GuildListIndicatorBarTarget;
	readonly backgroundImage: string | null;
	readonly iconBorderRadius: string;
	readonly mentionCount: number;
	readonly canManageGuild: boolean;
	readonly hasVoiceActivity: boolean;
	readonly voiceBadgeActivity: VoiceBadgeActivity | null;
	readonly guildARIALabel: string;
	readonly focusRingTargetRef: React.RefObject<HTMLElement | null>;
	readonly surfaceRef: React.RefCallback<HTMLElement>;
	readonly dragAndDrop: GuildListItemDragAndDrop | null;
	readonly onClick: () => void;
	readonly onContextMenu: (event: React.MouseEvent) => void;
	readonly onKeyDown: (event: React.KeyboardEvent) => void;
	readonly onLongPress: () => void;
}

type GuildListItemPresentationDomProps = Omit<
	React.HTMLAttributes<HTMLElement>,
	'onClick' | 'onContextMenu' | 'onKeyDown'
>;

export const GuildListItemPresentation = forwardRef<
	HTMLElement,
	GuildListItemPresentationProps & GuildListItemPresentationDomProps
>(function GuildListItemPresentation(
	{
		backgroundImage,
		canManageGuild,
		contextMenuOpen,
		dragAndDrop,
		focusRingTargetRef,
		guild,
		guildARIALabel,
		hasVoiceActivity,
		iconBorderRadius,
		indicatorTarget,
		isSelected,
		isSortingList,
		mentionCount,
		onClick,
		onContextMenu,
		onKeyDown,
		onLongPress,
		showGuildIndicator,
		surfaceRef,
		voiceBadgeActivity,
		...domProps
	},
	forwardedRef,
) {
	const rootRef = useMergeRefs([surfaceRef, forwardedRef]);
	const rawInitials = getInitialsFromName(guild.name);
	const initialsLength = resolveInitialsLength(rawInitials);
	const dropIndicator = resolveDropIndicator(dragAndDrop);
	const motion = resolveGuildListItemMotion(Accessibility.useReducedMotion);
	const draggingCursor = resolveDraggingCursor(dragAndDrop);
	const guildIconStyle = useMemo(
		() => resolveGuildIconStyle(backgroundImage, draggingCursor),
		[backgroundImage, draggingCursor],
	);
	const guildIconTarget = useMemo(() => ({borderRadius: iconBorderRadius}), [iconBorderRadius]);
	const hasPaintedIcon = backgroundImage != null;
	return (
		<LongPressable
			{...domProps}
			className={clsx(
				styles.guildListItem,
				styles.guildListReorderTarget,
				contextMenuOpen && styles.contextMenuHover,
				dropIndicator === Edge.TOP && styles.dropIndicatorTop,
				dropIndicator === Edge.BOTTOM && styles.dropIndicatorBottom,
				dropIndicator === DropPlacement.COMBINE && styles.dropIndicatorCombine,
			)}
			aria-label={guildARIALabel}
			onClick={onClick}
			onContextMenu={onContextMenu}
			onKeyDown={onKeyDown}
			ref={rootRef}
			role="button"
			tabIndex={0}
			data-guild-list-focus-item="true"
			data-guild-id={guild.id}
			onLongPress={onLongPress}
			disabled={false}
			data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item.click"
			{...ariaCurrentPage(isSelected)}
		>
			<AnimePresence data-flx="app.sidebar-nav.guild-list-item-presentation.anime-presence">
				{!isSortingList && showGuildIndicator && (
					<flx-app-guild-list-item-indicator
						className={flxElementClassName(styles.guildIndicator)}
						data-flx="app.sidebar-nav.guild-list-item-presentation.guild-indicator"
					>
						<AnimeSpan
							className={styles.guildIndicatorBar}
							from={false}
							to={indicatorTarget}
							leave={motion.indicatorLeave}
							tween={{duration: motion.indicatorTweenDuration, ease: [0.25, 0.1, 0.25, 1]}}
							data-flx="app.sidebar-nav.guild-list-item-presentation.guild-indicator-bar"
						/>
					</flx-app-guild-list-item-indicator>
				)}
			</AnimePresence>
			<flx-app-guild-list-item-frame
				className={flxElementClassName(styles.relative)}
				data-flx="app.sidebar-nav.guild-list-item-presentation.relative"
			>
				<GuildListItemIcon
					ref={focusRingTargetRef}
					tabIndex={-1}
					className={clsx(
						styles.guildIcon,
						!hasPaintedIcon && styles.guildIconNoImage,
						isSelected && styles.guildIconSelected,
					)}
					to={guildIconTarget}
					from={false}
					tween={{duration: motion.iconTweenDuration, ease: 'easeOut'}}
					data-initials-length={initialsLength}
					style={guildIconStyle}
					data-flx="app.sidebar-nav.guild-list-item-presentation.guild-icon"
				>
					{!hasPaintedIcon && (
						<span
							className={styles.guildIconInitials}
							data-flx="app.sidebar-nav.guild-list-item-presentation.guild-icon-initials"
						>
							{getGuildIconDisplayInitials(rawInitials)}
						</span>
					)}
				</GuildListItemIcon>
				<GuildListItemBadges
					guild={guild}
					mentionCount={mentionCount}
					canManageGuild={canManageGuild}
					hasVoiceActivity={hasVoiceActivity}
					voiceBadgeActivity={voiceBadgeActivity}
					data-flx="app.sidebar-nav.guild-list-item-presentation.guild-list-item-badges"
				/>
			</flx-app-guild-list-item-frame>
			{renderCombinePreview(guild, dragAndDrop, dropIndicator)}
		</LongPressable>
	);
});
