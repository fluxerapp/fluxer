// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/DMSidebarSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	getRememberedSkeletonDMSidebarLayout,
	type RememberedSkeletonDMSidebarLayout,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';
import {useMemo, useState} from 'react';

const QUICK_SWITCHER_HEIGHT = '1.75rem';
const FRIENDS_ACTION_LABEL_WIDTH = '4.5rem';
const PERSONAL_NOTES_ACTION_LABEL_WIDTH = '7.75rem';
const PREMIUM_ACTION_LABEL_WIDTH = '5.75rem';
const ACTION_ICON_SIZE = '2rem';
const ACTION_LABEL_HEIGHT = '0.75rem';
const SECTION_LABEL_WIDTH = '6rem';
const SECTION_LABEL_HEIGHT = '0.625rem';
const SECTION_ACTION_SIZE = '1rem';
const DEFAULT_CHANNEL_COUNT = 0;
const CHANNEL_AVATAR_SIZE = '2rem';
const CHANNEL_NAME_HEIGHT = '0.75rem';
const CHANNEL_SUBTEXT_HEIGHT = '0.5rem';
const CHANNEL_NAME_WIDTH_MIN = 42;
const CHANNEL_NAME_WIDTH_RANGE = 42;
const CHANNEL_SUBTEXT_WIDTH_MIN = 32;
const CHANNEL_SUBTEXT_WIDTH_RANGE = 38;
const CHANNEL_SUBTEXT_PROBABILITY = 0.45;
const MOBILE_TITLE_WIDTH = '5.5rem';
const MOBILE_TITLE_HEIGHT = '0.875rem';
const MOBILE_SEARCH_BUTTON_SIZE = '2rem';
const MOBILE_ADD_FRIEND_WIDTH = '7.5rem';
const MOBILE_ADD_FRIEND_HEIGHT = '2.25rem';
const MOBILE_ACTION_ICON_SIZE = '2.5rem';
const MOBILE_CHANNEL_AVATAR_SIZE = '2.5rem';
const MOBILE_TIMESTAMP_WIDTH = '1.5rem';
const MOBILE_TIMESTAMP_HEIGHT = '0.5rem';
const MOBILE_FAB_SIZE = '3.5rem';

interface DMChannelRowSpec {
	nameWidth: string;
	subtextWidth: string | undefined;
}

interface DMActionSkeletonSpec {
	readonly key: string;
	readonly labelWidth: string;
}

function createDMActionSkeletonSpecs(
	layout: RememberedSkeletonDMSidebarLayout | null,
	isMobile: boolean,
): ReadonlyArray<DMActionSkeletonSpec> {
	const specs: Array<DMActionSkeletonSpec> = [];
	if (!isMobile && (layout?.friendsVisible ?? true)) {
		specs.push({key: 'friends', labelWidth: FRIENDS_ACTION_LABEL_WIDTH});
	}
	if (layout?.personalNotesVisible ?? true) {
		specs.push({key: 'personal-notes', labelWidth: PERSONAL_NOTES_ACTION_LABEL_WIDTH});
	}
	if (layout?.premiumVisible ?? (!RuntimeConfig.isSelfHosted() && !isMobile)) {
		specs.push({key: 'premium', labelWidth: PREMIUM_ACTION_LABEL_WIDTH});
	}
	return Object.freeze(specs);
}

class DMSidebarSkeletonSpecOwner {
	private readonly random = createSkeletonRandomFromKey('dm-sidebar-skeleton-channels');

	public createChannels(
		channelCount: number,
		subtextFlags: ReadonlyArray<boolean> | null,
	): ReadonlyArray<DMChannelRowSpec> {
		return Array.from({length: channelCount}, (_unused, index) => this.createChannel(subtextFlags, index));
	}

	private createChannel(subtextFlags: ReadonlyArray<boolean> | null, index: number): DMChannelRowSpec {
		const nameWidth = `${CHANNEL_NAME_WIDTH_MIN + this.random() * CHANNEL_NAME_WIDTH_RANGE}%`;
		let hasSubtext = this.random() < CHANNEL_SUBTEXT_PROBABILITY;
		if (subtextFlags != null) {
			hasSubtext = subtextFlags[index] === true;
		}
		let subtextWidth: string | undefined;
		if (hasSubtext) {
			subtextWidth = `${CHANNEL_SUBTEXT_WIDTH_MIN + this.random() * CHANNEL_SUBTEXT_WIDTH_RANGE}%`;
		}
		return {nameWidth, subtextWidth};
	}
}

interface DMActionSkeletonRowProps {
	readonly index: number;
	readonly labelWidth: string;
	readonly mobile: boolean;
}

function DMActionSkeletonRow({index, labelWidth, mobile}: DMActionSkeletonRowProps) {
	let iconSize = ACTION_ICON_SIZE;
	let rowClassName = styles.row;
	let keyPrefix = 'dm-action';
	if (mobile) {
		iconSize = MOBILE_ACTION_ICON_SIZE;
		rowClassName = styles.mobileRow;
		keyPrefix = 'dm-mobile-action';
	}
	return (
		<flx-app-dm-sidebar-skeleton-action
			key={`${keyPrefix}-${index}`}
			className={flxElementClassName(rowClassName)}
			data-flx="app.skeleton.dm-sidebar-skeleton.dm-action-skeleton-row.flx-app-dm-sidebar-skeleton-action"
		>
			<flx-app-dm-sidebar-skeleton-action-content
				className={flxElementClassName(styles.rowContent)}
				data-flx="app.skeleton.dm-sidebar-skeleton.dm-action-skeleton-row.row-content"
			>
				<SkeletonCircle
					size={iconSize}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.dm-sidebar-skeleton.dm-action-skeleton-row.skeleton-circle"
				/>
				<SkeletonLine
					width={labelWidth}
					height={ACTION_LABEL_HEIGHT}
					data-flx="app.skeleton.dm-sidebar-skeleton.dm-action-skeleton-row.skeleton-line"
				/>
			</flx-app-dm-sidebar-skeleton-action-content>
		</flx-app-dm-sidebar-skeleton-action>
	);
}

interface DMChannelSkeletonRowProps {
	readonly channel: DMChannelRowSpec;
	readonly index: number;
	readonly mobile: boolean;
}

function DMChannelSkeletonRow({channel, index, mobile}: DMChannelSkeletonRowProps) {
	let avatarSize = CHANNEL_AVATAR_SIZE;
	let rowClassName = styles.row;
	let keyPrefix = 'dm-channel';
	if (mobile) {
		avatarSize = MOBILE_CHANNEL_AVATAR_SIZE;
		rowClassName = styles.mobileRow;
		keyPrefix = 'dm-mobile-channel';
	}
	return (
		<flx-app-dm-sidebar-skeleton-channel
			key={`${keyPrefix}-${index}`}
			className={flxElementClassName(rowClassName)}
			data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.flx-app-dm-sidebar-skeleton-channel"
		>
			<flx-app-dm-sidebar-skeleton-channel-content
				className={flxElementClassName(styles.rowContent)}
				data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.row-content"
			>
				<SkeletonCircle
					size={avatarSize}
					data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.skeleton-circle"
				/>
				<flx-app-dm-sidebar-skeleton-channel-info
					className={flxElementClassName(styles.channelInfo)}
					data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.channel-info"
				>
					<flx-app-dm-sidebar-skeleton-channel-name
						className={flxElementClassName(styles.channelNameRow)}
						data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.channel-name-row"
					>
						<SkeletonLine
							width={channel.nameWidth}
							height={CHANNEL_NAME_HEIGHT}
							data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.skeleton-line"
						/>
					</flx-app-dm-sidebar-skeleton-channel-name>
					{channel.subtextWidth != null && (
						<flx-app-dm-sidebar-skeleton-channel-subtext
							className={flxElementClassName(styles.channelSubtextRow)}
							data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.channel-subtext-row"
						>
							<SkeletonLine
								width={channel.subtextWidth}
								height={CHANNEL_SUBTEXT_HEIGHT}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.skeleton-line--2"
							/>
						</flx-app-dm-sidebar-skeleton-channel-subtext>
					)}
				</flx-app-dm-sidebar-skeleton-channel-info>
				{mobile && (
					<SkeletonLine
						width={MOBILE_TIMESTAMP_WIDTH}
						height={MOBILE_TIMESTAMP_HEIGHT}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row.skeleton-line--3"
					/>
				)}
			</flx-app-dm-sidebar-skeleton-channel-content>
		</flx-app-dm-sidebar-skeleton-channel>
	);
}

export const DMSidebarSkeleton = observer(() => {
	const [mountState] = useState(() => {
		const isMobile = MobileLayout.enabled;
		return Object.freeze({isMobile, rememberedLayout: getRememberedSkeletonDMSidebarLayout()});
	});
	const {isMobile, rememberedLayout} = mountState;
	const layout = rememberedLayout?.isMobile === isMobile ? rememberedLayout : null;
	const channelCount = layout?.channelRowCount ?? DEFAULT_CHANNEL_COUNT;
	const subtextFlags = layout?.channelSubtextFlags ?? null;
	const actionSpecs = createDMActionSkeletonSpecs(layout, isMobile);
	const channels = useMemo<ReadonlyArray<DMChannelRowSpec>>(
		() => new DMSidebarSkeletonSpecOwner().createChannels(channelCount, subtextFlags),
		[channelCount, subtextFlags],
	);
	if (isMobile) {
		return (
			<flx-app-dm-sidebar-skeleton
				className={flxElementClassName(styles.container, styles.containerMobile)}
				aria-hidden
				data-flx="app.skeleton.dm-sidebar-skeleton.container"
			>
				<flx-app-dm-sidebar-skeleton-mobile-header
					className={flxElementClassName(styles.mobileHeader)}
					data-flx="app.skeleton.dm-sidebar-skeleton.mobile-header"
				>
					<SkeletonLine
						width={MOBILE_TITLE_WIDTH}
						height={MOBILE_TITLE_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.dm-sidebar-skeleton.skeleton-line"
					/>
					<flx-app-dm-sidebar-skeleton-mobile-actions
						className={flxElementClassName(styles.mobileHeaderActions)}
						data-flx="app.skeleton.dm-sidebar-skeleton.mobile-header-actions"
					>
						<SkeletonCircle
							size={MOBILE_SEARCH_BUTTON_SIZE}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.dm-sidebar-skeleton.skeleton-circle"
						/>
						<SkeletonBlock
							width={MOBILE_ADD_FRIEND_WIDTH}
							height={MOBILE_ADD_FRIEND_HEIGHT}
							radius={SkeletonRadius.PILL}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.dm-sidebar-skeleton.skeleton-block"
						/>
					</flx-app-dm-sidebar-skeleton-mobile-actions>
				</flx-app-dm-sidebar-skeleton-mobile-header>
				<flx-app-dm-sidebar-skeleton-scroll-area
					className={flxElementClassName(styles.scrollArea)}
					data-flx="app.skeleton.dm-sidebar-skeleton.scroll-area"
				>
					<flx-app-dm-sidebar-skeleton-content
						className={flxElementClassName(styles.mobileContent)}
						data-flx="app.skeleton.dm-sidebar-skeleton.mobile-content"
					>
						{actionSpecs.map((action, index) => (
							<DMActionSkeletonRow
								key={action.key}
								index={index}
								labelWidth={action.labelWidth}
								mobile
								data-flx="app.skeleton.dm-sidebar-skeleton.dm-action-skeleton-row"
							/>
						))}
						{channels.map((channel, index) => (
							<DMChannelSkeletonRow
								key={index}
								channel={channel}
								index={index}
								mobile
								data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row"
							/>
						))}
						<flx-app-dm-sidebar-skeleton-bottom-spacer
							className={flxElementClassName(styles.spacer)}
							data-flx="app.skeleton.dm-sidebar-skeleton.spacer"
						/>
					</flx-app-dm-sidebar-skeleton-content>
				</flx-app-dm-sidebar-skeleton-scroll-area>
				<SkeletonCircle
					size={MOBILE_FAB_SIZE}
					emphasis={SkeletonEmphasis.STRONG}
					className={styles.mobileFab}
					data-flx="app.skeleton.dm-sidebar-skeleton.mobile-fab"
				/>
			</flx-app-dm-sidebar-skeleton>
		);
	}
	return (
		<flx-app-dm-sidebar-skeleton
			className={flxElementClassName(styles.container)}
			aria-hidden
			data-flx="app.skeleton.dm-sidebar-skeleton.container--2"
		>
			<flx-app-dm-sidebar-skeleton-header
				className={flxElementClassName(styles.header)}
				data-flx="app.skeleton.dm-sidebar-skeleton.header"
			>
				<SkeletonBlock
					height={QUICK_SWITCHER_HEIGHT}
					radius={SkeletonRadius.MEDIUM}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.dm-sidebar-skeleton.skeleton-block--2"
				/>
			</flx-app-dm-sidebar-skeleton-header>
			<flx-app-dm-sidebar-skeleton-scroll-area
				className={flxElementClassName(styles.scrollArea)}
				data-flx="app.skeleton.dm-sidebar-skeleton.scroll-area--2"
			>
				<flx-app-dm-sidebar-skeleton-content
					className={flxElementClassName(styles.content)}
					data-flx="app.skeleton.dm-sidebar-skeleton.content"
				>
					<flx-app-dm-sidebar-skeleton-top-spacer
						className={flxElementClassName(styles.spacer)}
						data-flx="app.skeleton.dm-sidebar-skeleton.spacer--2"
					/>
					{actionSpecs.map((action, index) => (
						<DMActionSkeletonRow
							key={action.key}
							index={index}
							labelWidth={action.labelWidth}
							mobile={false}
							data-flx="app.skeleton.dm-sidebar-skeleton.dm-action-skeleton-row--2"
						/>
					))}
					{(layout?.sectionVisible ?? true) && (
						<>
							<flx-app-dm-sidebar-skeleton-separator
								className={flxElementClassName(styles.separator)}
								data-flx="app.skeleton.dm-sidebar-skeleton.separator"
							/>
							<flx-app-dm-sidebar-skeleton-section-header
								className={flxElementClassName(styles.sectionHeader)}
								data-flx="app.skeleton.dm-sidebar-skeleton.section-header"
							>
								<SkeletonLine
									width={SECTION_LABEL_WIDTH}
									height={SECTION_LABEL_HEIGHT}
									emphasis={SkeletonEmphasis.MUTED}
									data-flx="app.skeleton.dm-sidebar-skeleton.skeleton-line--2"
								/>
								<SkeletonBlock
									width={SECTION_ACTION_SIZE}
									height={SECTION_ACTION_SIZE}
									radius={SkeletonRadius.SMALL}
									emphasis={SkeletonEmphasis.MUTED}
									data-flx="app.skeleton.dm-sidebar-skeleton.skeleton-block--3"
								/>
							</flx-app-dm-sidebar-skeleton-section-header>
						</>
					)}
					<flx-app-dm-sidebar-skeleton-channel-list
						className={flxElementClassName(styles.channelList)}
						data-flx="app.skeleton.dm-sidebar-skeleton.channel-list"
					>
						{channels.map((channel, index) => (
							<DMChannelSkeletonRow
								key={index}
								channel={channel}
								index={index}
								mobile={false}
								data-flx="app.skeleton.dm-sidebar-skeleton.dm-channel-skeleton-row--2"
							/>
						))}
					</flx-app-dm-sidebar-skeleton-channel-list>
					<flx-app-dm-sidebar-skeleton-bottom-spacer
						className={flxElementClassName(styles.bottomSpacer)}
						data-flx="app.skeleton.dm-sidebar-skeleton.bottom-spacer"
					/>
				</flx-app-dm-sidebar-skeleton-content>
			</flx-app-dm-sidebar-skeleton-scroll-area>
		</flx-app-dm-sidebar-skeleton>
	);
});
