// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/ChatSkeleton.module.css';
import {usePlaceholderSpecs} from '@app/features/app/components/skeleton/PlaceholderSpecs';
import {
	ChatSkeletonChannelKind,
	type ChatSkeletonPresentation,
} from '@app/features/app/components/skeleton/ResolveChatSkeleton';
import {ScrollFillerSkeleton} from '@app/features/app/components/skeleton/ScrollFillerSkeleton';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	getRememberedSkeletonComposerLayout,
	getRememberedSkeletonMessagePresentation,
	resolveDefaultSkeletonComposerLayout,
	resolveDefaultSkeletonMessagePresentation,
	SKELETON_UNMEASURED_WIDTH_PX,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {
	type SkeletonInjectedToken,
	skeletonSurfaceVar,
} from '@app/features/app/components/skeleton/SkeletonSurfaceContract';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {CHANNEL_HEADER_DM_AVATAR_SIZE_PX} from '@app/features/channel/components/ChannelHeaderMetrics';
import {MemberListSkeleton, MemberListSkeletonVariant} from '@app/features/channel/components/MemberListSkeleton';
import composerWrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import composerInputStyles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Fragment, useState} from 'react';

const MESSAGE_LIST_SEED_KEY = 'chat-skeleton-message-list';

const HEADER_BACK_ICON_SIZE = skeletonSurfaceVar('--channel-header-back-icon-size');
const HEADER_ICON_SIZE = skeletonSurfaceVar('--channel-header-icon-size');
const HEADER_DM_AVATAR_SIZE = remFromPx(CHANNEL_HEADER_DM_AVATAR_SIZE_PX);
const HEADER_NAME_FALLBACK_WIDTH = '7.5rem';
const HEADER_NAME_HEIGHT = '0.875rem';
const HEADER_CARET_SIZE = skeletonSurfaceVar('--channel-header-caret-size');
const HEADER_TOPIC_DIVIDER_SIZE = '0.25rem';
const HEADER_TOPIC_FALLBACK_WIDTH = '18rem';
const HEADER_TOPIC_HEIGHT = '0.6875rem';
const HEADER_ACTION_ICON_SIZE = skeletonSurfaceVar('--channel-header-action-icon-size');
const HEADER_SEARCH_ICON_SIZE = skeletonSurfaceVar('--message-search-bar-icon-size');
const HEADER_SEARCH_PLACEHOLDER_WIDTH = '7rem';
const HEADER_SEARCH_PLACEHOLDER_HEIGHT = '0.6875rem';
const HEADER_MOBILE_ACTION_SIZE = skeletonSurfaceVar('--channel-header-action-size-mobile');

const COMPOSER_ICON_SIZE = skeletonSurfaceVar('--textarea-button-icon-size');
const COMPOSER_PLACEHOLDER_WIDTH = '11rem';
const COMPOSER_PLACEHOLDER_HEIGHT = '0.875rem';

type ChatSkeletonRootToken = Extract<
	SkeletonInjectedToken,
	'--chat-horizontal-padding' | '--font-size' | '--message-group-spacing' | '--message-compact-timestamp-width'
>;

type ChatSkeletonRootStyle = React.CSSProperties & Record<ChatSkeletonRootToken, string>;

function resolveMeasuredWidth(widthPx: number, fallback: string): string {
	if (widthPx === SKELETON_UNMEASURED_WIDTH_PX) {
		return fallback;
	}
	return remFromPx(widthPx);
}

function HeaderActionSlot() {
	return (
		<flx-chat-skeleton-header-action
			className={flxElementClassName(styles.headerAction)}
			data-flx="app.skeleton.chat-skeleton.header-action-slot.header-action"
		>
			<SkeletonBlock
				width={HEADER_ACTION_ICON_SIZE}
				height={HEADER_ACTION_ICON_SIZE}
				radius={SkeletonRadius.SMALL}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.chat-skeleton.header-action-slot.skeleton-block"
			/>
		</flx-chat-skeleton-header-action>
	);
}

function HeaderSearchSkeleton() {
	return (
		<flx-chat-skeleton-header-search
			className={flxElementClassName(styles.headerSearch)}
			data-flx="app.skeleton.chat-skeleton.header-search-skeleton.header-search"
		>
			<SkeletonBlock
				width={HEADER_SEARCH_ICON_SIZE}
				height={HEADER_SEARCH_ICON_SIZE}
				radius={SkeletonRadius.SMALL}
				emphasis={SkeletonEmphasis.MUTED}
				className={styles.headerSearchIcon}
				data-flx="app.skeleton.chat-skeleton.header-search-skeleton.header-search-icon"
			/>
			<SkeletonLine
				width={HEADER_SEARCH_PLACEHOLDER_WIDTH}
				height={HEADER_SEARCH_PLACEHOLDER_HEIGHT}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.chat-skeleton.header-search-skeleton.skeleton-line"
			/>
		</flx-chat-skeleton-header-search>
	);
}

interface HeaderActionProjection {
	readonly desktopLeadingCount: number;
	readonly mobileCount: number;
	readonly showSearch: boolean;
	readonly persistentDesktopActionCount: number;
}

function resolveDefaultHeaderActionCounts(
	channelKind: ChatSkeletonChannelKind,
	favoriteCount: number,
): {desktopLeadingCount: number; mobileCount: number; showSearch: boolean} {
	switch (channelKind) {
		case ChatSkeletonChannelKind.DM:
			return {desktopLeadingCount: 4 + favoriteCount, mobileCount: 2 + favoriteCount, showSearch: true};
		case ChatSkeletonChannelKind.GROUP_DM:
			return {desktopLeadingCount: 5 + favoriteCount, mobileCount: 2 + favoriteCount, showSearch: true};
		case ChatSkeletonChannelKind.PERSONAL_NOTES:
			return {desktopLeadingCount: 0, mobileCount: 0, showSearch: false};
		case ChatSkeletonChannelKind.GUILD:
		case ChatSkeletonChannelKind.GUILD_VOICE:
			return {desktopLeadingCount: 3 + favoriteCount, mobileCount: 1 + favoriteCount, showSearch: true};
	}
}

function resolveHeaderActionProjection(presentation: ChatSkeletonPresentation): HeaderActionProjection {
	let favoriteCount = 0;
	if (presentation.favoritesVisible) {
		favoriteCount = 1;
	}
	const defaults = resolveDefaultHeaderActionCounts(presentation.channelKind, favoriteCount);
	let persistentDesktopActionCount = 0;
	if (presentation.staffToolsVisible) {
		persistentDesktopActionCount += 1;
	}
	if (presentation.updaterVisible) {
		persistentDesktopActionCount += 1;
	}
	return {
		desktopLeadingCount: presentation.headerDesktopLeadingActionCount ?? defaults.desktopLeadingCount,
		mobileCount: presentation.headerMobileActionCount ?? defaults.mobileCount,
		showSearch: defaults.showSearch,
		persistentDesktopActionCount,
	};
}

interface ChatSkeletonProps {
	readonly presentation: ChatSkeletonPresentation;
}

export const ChatSkeleton = observer(function ChatSkeleton({presentation}: ChatSkeletonProps) {
	const isMobile = MobileLayout.enabled;
	const {channelKind, showMemberList, showTopic, rememberedMemberGroups} = presentation;
	const [rememberedChatState] = useState(() =>
		Object.freeze({
			composerLayout:
				getRememberedSkeletonComposerLayout() ?? resolveDefaultSkeletonComposerLayout(RuntimeConfig.gifEnabled),
			messagePresentation: getRememberedSkeletonMessagePresentation() ?? resolveDefaultSkeletonMessagePresentation(),
		}),
	);
	const {composerLayout, messagePresentation} = rememberedChatState;
	let composerActionCount = composerLayout.desktopActionCount;
	let composerDividerVisible = composerLayout.sendDividerVisible;
	if (isMobile) {
		composerActionCount = composerLayout.mobileActionCount;
		composerDividerVisible = false;
	}
	const messageSpecs = usePlaceholderSpecs({
		compact: messagePresentation.compact,
		compactAvatarsVisible: messagePresentation.compactAvatarsVisible,
		groupSpacing: messagePresentation.groupSpacingPx,
		viewportHeightPx: messagePresentation.viewportHeightPx,
		seedKey: MESSAGE_LIST_SEED_KEY,
	});
	const rootStyle: ChatSkeletonRootStyle = {
		'--chat-horizontal-padding': remFromPx(messagePresentation.messageGutterPx),
		'--font-size': remFromPx(messagePresentation.fontSizePx),
		'--message-group-spacing': remFromPx(messagePresentation.groupSpacingPx),
		'--message-compact-timestamp-width': remFromPx(messagePresentation.compactTimestampWidthPx),
	};
	const headerActionProjection = resolveHeaderActionProjection(presentation);
	const renderHeaderIdentityIcon = (): React.ReactNode => {
		if (channelKind === ChatSkeletonChannelKind.DM || channelKind === ChatSkeletonChannelKind.GROUP_DM) {
			return (
				<SkeletonCircle
					size={HEADER_DM_AVATAR_SIZE}
					data-flx="app.skeleton.chat-skeleton.render-header-identity-icon.skeleton-circle"
				/>
			);
		}
		return (
			<SkeletonBlock
				width={HEADER_ICON_SIZE}
				height={HEADER_ICON_SIZE}
				radius={SkeletonRadius.SMALL}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.chat-skeleton.render-header-identity-icon.skeleton-block"
			/>
		);
	};
	const renderHeaderIdentityTail = (): React.ReactNode => {
		if (isMobile) {
			return (
				<SkeletonBlock
					width={HEADER_CARET_SIZE}
					height={HEADER_CARET_SIZE}
					radius={SkeletonRadius.SMALL}
					emphasis={SkeletonEmphasis.MUTED}
					className={styles.identityCaret}
					data-flx="app.skeleton.chat-skeleton.render-header-identity-tail.identity-caret"
				/>
			);
		}
		if (showTopic) {
			return (
				<>
					<SkeletonCircle
						size={HEADER_TOPIC_DIVIDER_SIZE}
						emphasis={SkeletonEmphasis.MUTED}
						className={styles.identityTopicDivider}
						data-flx="app.skeleton.chat-skeleton.render-header-identity-tail.identity-topic-divider"
					/>
					<SkeletonLine
						width={resolveMeasuredWidth(presentation.headerTopicWidthPx, HEADER_TOPIC_FALLBACK_WIDTH)}
						height={HEADER_TOPIC_HEIGHT}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.chat-skeleton.render-header-identity-tail.skeleton-line"
					/>
				</>
			);
		}
		return null;
	};
	const renderHeaderActions = (): React.ReactNode => {
		if (isMobile) {
			return Array.from(Array(headerActionProjection.mobileCount).keys(), (index) => (
				<SkeletonCircle
					key={`chat-skeleton-header-mobile-action-${index}`}
					size={HEADER_MOBILE_ACTION_SIZE}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.chat-skeleton.render-header-actions.skeleton-circle"
				/>
			));
		}
		return (
			<>
				{Array.from(Array(headerActionProjection.desktopLeadingCount).keys(), (index) => (
					<HeaderActionSlot
						key={`chat-skeleton-header-action-${index}`}
						data-flx="app.skeleton.chat-skeleton.render-header-actions.header-action-slot"
					/>
				))}
				{headerActionProjection.showSearch && (
					<HeaderSearchSkeleton data-flx="app.skeleton.chat-skeleton.render-header-actions.header-search-skeleton" />
				)}
				{Array.from(Array(headerActionProjection.persistentDesktopActionCount).keys(), (index) => (
					<HeaderActionSlot
						key={`chat-skeleton-header-persistent-action-${index}`}
						data-flx="app.skeleton.chat-skeleton.render-header-actions.header-action-slot--2"
					/>
				))}
				<HeaderActionSlot data-flx="app.skeleton.chat-skeleton.render-header-actions.header-action-slot--3" />
			</>
		);
	};
	const renderComposerActions = (): React.ReactNode => {
		const dividerIndex = composerDividerVisible ? composerActionCount - 1 : -1;
		return Array.from(Array(composerActionCount).keys(), (index) => (
			<Fragment key={`chat-skeleton-composer-action-${index}`}>
				{index === dividerIndex && (
					<flx-chat-skeleton-composer-divider
						className={flxElementClassName(composerInputStyles.divider)}
						data-flx="app.skeleton.chat-skeleton.render-composer-actions.flx-chat-skeleton-composer-divider"
					/>
				)}
				<flx-chat-skeleton-composer-action
					className={flxElementClassName(styles.composerAction)}
					data-flx="app.skeleton.chat-skeleton.render-composer-actions.composer-action"
				>
					<SkeletonBlock
						width={COMPOSER_ICON_SIZE}
						height={COMPOSER_ICON_SIZE}
						radius={SkeletonRadius.MEDIUM}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.chat-skeleton.render-composer-actions.skeleton-block"
					/>
				</flx-chat-skeleton-composer-action>
			</Fragment>
		));
	};
	return (
		<flx-chat-skeleton
			className={flxElementClassName(styles.grid)}
			style={rootStyle}
			aria-hidden
			data-flx="app.skeleton.chat-skeleton.grid"
		>
			<flx-chat-skeleton-header
				className={flxElementClassName(styles.header)}
				data-flx="app.skeleton.chat-skeleton.header"
			>
				<flx-chat-skeleton-identity
					className={flxElementClassName(styles.identity)}
					data-flx="app.skeleton.chat-skeleton.identity"
				>
					<flx-chat-skeleton-back
						className={flxElementClassName(styles.identityBack, !isMobile && styles.identityBackDesktop)}
						data-flx="app.skeleton.chat-skeleton.identity-back"
					>
						<SkeletonBlock
							width={HEADER_BACK_ICON_SIZE}
							height={HEADER_BACK_ICON_SIZE}
							radius={SkeletonRadius.SMALL}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.chat-skeleton.skeleton-block"
						/>
					</flx-chat-skeleton-back>
					{renderHeaderIdentityIcon()}
					<SkeletonLine
						width={resolveMeasuredWidth(presentation.headerNameWidthPx, HEADER_NAME_FALLBACK_WIDTH)}
						height={HEADER_NAME_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						className={flxElementClassName(
							styles.identityName,
							!isMobile && channelKind === ChatSkeletonChannelKind.GROUP_DM && styles.identityNameGroupDM,
						)}
						data-flx="app.skeleton.chat-skeleton.identity-name"
					/>
					{renderHeaderIdentityTail()}
				</flx-chat-skeleton-identity>
				<flx-chat-skeleton-header-actions
					className={flxElementClassName(styles.headerActions)}
					data-flx="app.skeleton.chat-skeleton.header-actions"
				>
					{renderHeaderActions()}
				</flx-chat-skeleton-header-actions>
			</flx-chat-skeleton-header>
			<flx-chat-skeleton-content
				className={flxElementClassName(styles.content)}
				data-flx="app.skeleton.chat-skeleton.content"
			>
				{showMemberList && (
					<flx-chat-skeleton-member-divider
						className={flxElementClassName(styles.memberListDivider)}
						data-flx="app.skeleton.chat-skeleton.member-list-divider"
					/>
				)}
				<flx-chat-skeleton-chat
					className={flxElementClassName(styles.chatArea)}
					data-flx="app.skeleton.chat-skeleton.chat-area"
				>
					<flx-chat-skeleton-chat-body
						className={flxElementClassName(styles.chatBody)}
						data-flx="app.skeleton.chat-skeleton.chat-body"
					>
						<flx-chat-skeleton-messages
							className={flxElementClassName(styles.messages)}
							data-flx="app.skeleton.chat-skeleton.messages"
						>
							<ScrollFillerSkeleton data-flx="app.skeleton.chat-skeleton.scroll-filler-skeleton" {...messageSpecs} />
						</flx-chat-skeleton-messages>
						<flx-chat-skeleton-composer
							className={flxElementClassName(
								composerWrapperStyles.box,
								composerWrapperStyles.composerRoot,
								composerWrapperStyles.wrapperSides,
								composerInputStyles.textareaOuter,
								isMobile && composerInputStyles.textareaOuterMobile,
								composerWrapperStyles.roundedAll,
								!isMobile && composerInputStyles.textareaOuterRow,
							)}
							data-flx="app.skeleton.chat-skeleton.flx-chat-skeleton-composer"
						>
							<flx-chat-skeleton-composer-section
								className={flxElementClassName(composerWrapperStyles.stackSection, composerInputStyles.inputSection)}
								data-flx="app.skeleton.chat-skeleton.flx-chat-skeleton-composer-section"
							>
								<flx-chat-skeleton-composer-grid
									className={flxElementClassName(composerInputStyles.mainWrapperDense)}
									data-flx="app.skeleton.chat-skeleton.flx-chat-skeleton-composer-grid"
								>
									<flx-chat-skeleton-composer-upload
										className={flxElementClassName(
											composerInputStyles.uploadButtonColumn,
											composerInputStyles.sideButtonPadding,
										)}
										data-flx="app.skeleton.chat-skeleton.flx-chat-skeleton-composer-upload"
									>
										<flx-chat-skeleton-composer-action
											className={flxElementClassName(styles.composerAction)}
											data-flx="app.skeleton.chat-skeleton.composer-action"
										>
											<SkeletonBlock
												width={COMPOSER_ICON_SIZE}
												height={COMPOSER_ICON_SIZE}
												radius={SkeletonRadius.MEDIUM}
												emphasis={SkeletonEmphasis.MUTED}
												data-flx="app.skeleton.chat-skeleton.skeleton-block--2"
											/>
										</flx-chat-skeleton-composer-action>
									</flx-chat-skeleton-composer-upload>
									<flx-chat-skeleton-composer-input
										className={flxElementClassName(composerInputStyles.contentAreaDense)}
										data-flx="app.skeleton.chat-skeleton.flx-chat-skeleton-composer-input"
									>
										<flx-chat-skeleton-composer-input-line
											className={flxElementClassName(styles.composerInputLine)}
											data-flx="app.skeleton.chat-skeleton.composer-input-line"
										>
											<SkeletonLine
												width={COMPOSER_PLACEHOLDER_WIDTH}
												height={COMPOSER_PLACEHOLDER_HEIGHT}
												emphasis={SkeletonEmphasis.MUTED}
												data-flx="app.skeleton.chat-skeleton.skeleton-line"
											/>
										</flx-chat-skeleton-composer-input-line>
									</flx-chat-skeleton-composer-input>
									<flx-chat-skeleton-composer-actions
										className={flxElementClassName(
											composerInputStyles.buttonContainerDense,
											composerInputStyles.sideButtonPadding,
										)}
										data-flx="app.skeleton.chat-skeleton.flx-chat-skeleton-composer-actions"
									>
										{renderComposerActions()}
									</flx-chat-skeleton-composer-actions>
								</flx-chat-skeleton-composer-grid>
							</flx-chat-skeleton-composer-section>
						</flx-chat-skeleton-composer>
					</flx-chat-skeleton-chat-body>
				</flx-chat-skeleton-chat>
				{showMemberList && (
					<flx-chat-skeleton-member-list
						className={flxElementClassName(
							styles.memberList,
							channelKind === ChatSkeletonChannelKind.GROUP_DM && styles.memberListGroupDM,
						)}
						data-flx="app.skeleton.chat-skeleton.member-list"
					>
						<flx-chat-skeleton-member-list-scroller
							className={flxElementClassName(styles.memberListScroller)}
							data-flx="app.skeleton.chat-skeleton.member-list-scroller"
						>
							<MemberListSkeleton
								variant={
									channelKind === ChatSkeletonChannelKind.GROUP_DM
										? MemberListSkeletonVariant.GROUP_DM
										: MemberListSkeletonVariant.GUILD
								}
								memberGroups={rememberedMemberGroups}
								data-flx="app.skeleton.chat-skeleton.member-list-skeleton"
							/>
						</flx-chat-skeleton-member-list-scroller>
					</flx-chat-skeleton-member-list>
				)}
			</flx-chat-skeleton-content>
		</flx-chat-skeleton>
	);
});
