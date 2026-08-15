// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUnreadChannels, UnreadChannelsContent} from '@app/features/app/components/floating/UnreadChannelsContent';
import {MENTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InboxCommands from '@app/features/inbox/commands/InboxCommands';
import Inbox, {type InboxTab} from '@app/features/inbox/state/Inbox';
import styles from '@app/features/messaging/components/popouts/InboxPopout.module.css';
import {RecentMentionsContent} from '@app/features/messaging/components/popouts/RecentMentionsContent';
import {SavedMessagesContent} from '@app/features/messaging/components/popouts/SavedMessagesContent';
import {ScheduledMessagesContent} from '@app/features/messaging/components/popouts/ScheduledMessagesContent';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {ResizeEdge} from '@app/features/ui/floating_pane/FloatingPaneMath';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {RESIZABLE_PANE_DEFAULT_VIEWPORT_PADDING, useResizablePane} from '@app/features/ui/hooks/useResizablePane';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AtIcon, BellIcon, BookmarkSimpleIcon, CheckIcon, ClockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const UNREAD_DESCRIPTOR = msg({
	message: 'Unread',
	comment: 'Tab label in the inbox popout filtering to channels with unread messages.',
});
const BOOKMARKS_DESCRIPTOR = msg({
	message: 'Bookmarks',
	comment: 'Tab label in the inbox popout listing the user bookmarks.',
});
const SCHEDULED_DESCRIPTOR = msg({
	message: 'Scheduled',
	comment: 'Tab label in the inbox popout listing the user scheduled messages.',
});
const INBOX_TABS_DESCRIPTOR = msg({
	message: 'Inbox tabs',
	comment: 'Accessible label for the inbox popout tablist.',
});
const MARK_ALL_AS_READ_DESCRIPTOR = msg({
	message: 'Mark all as read',
	comment: 'Action label in the inbox popout overflow menu. Marks every inbox channel as read.',
});
const MARK_ALL_INBOX_CHANNELS_AS_READ_DESCRIPTOR = msg({
	message: 'Mark all inbox channels as read',
	comment: 'Accessible label and tooltip for the mark-all-as-read button in the inbox popout.',
});
const RESIZE_INBOX_TOP_LEFT_DESCRIPTOR = msg({
	message: 'Resize inbox from top left',
	comment: 'Accessible label for the top-left resize handle on the inbox popout.',
});
const RESIZE_INBOX_TOP_RIGHT_DESCRIPTOR = msg({
	message: 'Resize inbox from top right',
	comment: 'Accessible label for the top-right resize handle on the inbox popout.',
});
const RESIZE_INBOX_BOTTOM_LEFT_DESCRIPTOR = msg({
	message: 'Resize inbox from bottom left',
	comment: 'Accessible label for the bottom-left resize handle on the inbox popout.',
});
const RESIZE_INBOX_BOTTOM_RIGHT_DESCRIPTOR = msg({
	message: 'Resize inbox from bottom right',
	comment: 'Accessible label for the bottom-right resize handle on the inbox popout.',
});

const INBOX_POPOUT_DEFAULT_SIZE = {width: 600, height: 900};
const INBOX_POPOUT_MIN_SIZE = {width: 440, height: 400};
const INBOX_POPOUT_RESIZING_CLASS = 'inbox-popout-resizing';
const INBOX_POPOUT_RESIZE_CURSOR_PROPERTY = '--inbox-popout-resize-cursor';
const INBOX_RESIZE_HANDLES: ReadonlyArray<{edge: ResizeEdge; className: string; label: MessageDescriptor}> = [
	{edge: 'top-left', className: styles.resizeCornerTopLeft, label: RESIZE_INBOX_TOP_LEFT_DESCRIPTOR},
	{edge: 'top-right', className: styles.resizeCornerTopRight, label: RESIZE_INBOX_TOP_RIGHT_DESCRIPTOR},
	{edge: 'bottom-left', className: styles.resizeCornerBottomLeft, label: RESIZE_INBOX_BOTTOM_LEFT_DESCRIPTOR},
	{edge: 'bottom-right', className: styles.resizeCornerBottomRight, label: RESIZE_INBOX_BOTTOM_RIGHT_DESCRIPTOR},
];

interface TabConfig {
	key: InboxTab;
	label: string;
	icon: React.ReactNode;
}

export const InboxPopout = observer(({initialTab}: {initialTab?: InboxTab} = {}) => {
	const {i18n} = useLingui();
	let activeTab = Inbox.selectedTab;
	if (initialTab != null) activeTab = initialTab;
	const [headerActions, setHeaderActions] = useState<React.ReactNode>(null);
	const containerRef = useRef<HTMLDivElement | null>(null);
	const {size, getHandleProps} = useResizablePane(containerRef, {
		storageKey: 'fluxer:ui:inbox-popout-size',
		defaultSize: INBOX_POPOUT_DEFAULT_SIZE,
		minSize: INBOX_POPOUT_MIN_SIZE,
		viewportPadding: RESIZABLE_PANE_DEFAULT_VIEWPORT_PADDING,
		resizingClassName: INBOX_POPOUT_RESIZING_CLASS,
		cursorProperty: INBOX_POPOUT_RESIZE_CURSOR_PROPERTY,
	});
	const readStateVersion = ReadStates.version;
	const settingsVersion = UserGuildSettings.version;
	const unreadChannels = useMemo(() => getUnreadChannels(), [readStateVersion, settingsVersion]);
	const baseTabs: Array<TabConfig> = [
		{
			key: 'unreadChannels',
			label: i18n._(UNREAD_DESCRIPTOR),
			icon: <BellIcon weight="fill" className={styles.iconSmall} data-flx="messaging.inbox-popout.icon-small" />,
		},
		{
			key: 'bookmarks',
			label: i18n._(BOOKMARKS_DESCRIPTOR),
			icon: <BookmarkSimpleIcon className={styles.iconSmall} data-flx="messaging.inbox-popout.icon-small--2" />,
		},
		{
			key: 'mentions',
			label: i18n._(MENTIONS_DESCRIPTOR),
			icon: <AtIcon weight="bold" className={styles.iconSmall} data-flx="messaging.inbox-popout.icon-small--3" />,
		},
	];
	const scheduledTab: TabConfig = {
		key: 'scheduled',
		label: i18n._(SCHEDULED_DESCRIPTOR),
		icon: <ClockIcon className={styles.iconSmall} data-flx="messaging.inbox-popout.icon-small--4" />,
	};
	const currentUser = Users.getCurrentUser();
	let showScheduledTab = false;
	if (currentUser != null) showScheduledTab = currentUser.isStaff();
	const tabs = showScheduledTab ? [...baseTabs, scheduledTab] : baseTabs;
	const normalizedActiveTab = tabs.some((tab) => tab.key === activeTab) ? activeTab : tabs[0].key;
	const setActiveTab = useCallback((tab: InboxTab) => {
		InboxCommands.setTab(tab);
	}, []);
	const focusTab = useCallback((tab: InboxTab) => {
		InboxCommands.setTab(tab);
		const container = containerRef.current;
		if (container == null) return;
		const ownerDocument = container.ownerDocument;
		const ownerWindow = ownerDocument.defaultView;
		if (ownerWindow == null) return;
		ownerWindow.requestAnimationFrame(() => {
			const tabElement = ownerDocument.getElementById(tab);
			if (tabElement != null) tabElement.focus();
		});
	}, []);
	const handleMarkAllRead = useCallback(() => {
		InboxCommands.markAllInboxChannelsAsRead(i18n);
	}, [i18n]);
	const renderTabButton = (tab: TabConfig) => {
		const isActive = normalizedActiveTab === tab.key;
		const tabIndex = tabs.findIndex((candidate) => candidate.key === tab.key);
		return (
			<Tooltip
				key={tab.key}
				text={tab.label}
				position="right"
				data-flx="messaging.inbox-popout.render-tab-button.tooltip"
			>
				<FocusRing offset={-2} data-flx="messaging.inbox-popout.render-tab-button.focus-ring">
					<button
						id={tab.key}
						role="tab"
						type="button"
						aria-label={tab.label}
						aria-selected={isActive}
						aria-controls={`inbox-panel-${tab.key}`}
						tabIndex={isActive ? 0 : -1}
						className={clsx(styles.sidebarTab, isActive ? styles.tabActive : styles.tabInactive)}
						onClick={() => setActiveTab(tab.key)}
						onKeyDown={(event) => {
							const direction = getTabNavigationDirection(event.key, 'both');
							if (!direction) return;
							const nextIndex = getNextTabIndex(tabIndex, tabs.length, direction);
							if (nextIndex == null) return;
							event.preventDefault();
							event.stopPropagation();
							const nextTab = tabs[nextIndex];
							if (!nextTab) return;
							focusTab(nextTab.key);
						}}
						data-flx="messaging.inbox-popout.render-tab-button.sidebar-tab.set-active-tab.button"
					>
						{tab.icon}
					</button>
				</FocusRing>
			</Tooltip>
		);
	};
	const content = (
		<div className={styles.content} data-flx="messaging.inbox-popout.content">
			{normalizedActiveTab === 'bookmarks' && (
				<div
					id="inbox-panel-bookmarks"
					className={styles.tabContent}
					role="tabpanel"
					tabIndex={-1}
					aria-labelledby="bookmarks"
					data-autofocus=""
					data-flx="messaging.inbox-popout.inbox-panel-bookmarks"
				>
					<SavedMessagesContent data-flx="messaging.inbox-popout.saved-messages-content" />
				</div>
			)}
			{normalizedActiveTab === 'unreadChannels' && (
				<div
					id="inbox-panel-unreadChannels"
					className={styles.tabContent}
					role="tabpanel"
					tabIndex={-1}
					aria-labelledby="unreadChannels"
					data-autofocus=""
					data-flx="messaging.inbox-popout.inbox-panel-unread-channels"
				>
					<UnreadChannelsContent data-flx="messaging.inbox-popout.unread-channels-content" />
				</div>
			)}
			{normalizedActiveTab === 'mentions' && (
				<div
					id="inbox-panel-mentions"
					className={styles.tabContent}
					role="tabpanel"
					tabIndex={-1}
					aria-labelledby="mentions"
					data-autofocus=""
					data-flx="messaging.inbox-popout.inbox-panel-mentions"
				>
					<RecentMentionsContent
						onHeaderActionsChange={setHeaderActions}
						data-flx="messaging.inbox-popout.recent-mentions-content"
					/>
				</div>
			)}
			{showScheduledTab && normalizedActiveTab === 'scheduled' && (
				<div
					id="inbox-panel-scheduled"
					className={styles.tabContent}
					role="tabpanel"
					tabIndex={-1}
					aria-labelledby="scheduled"
					data-autofocus=""
					data-flx="messaging.inbox-popout.inbox-panel-scheduled"
				>
					<ScheduledMessagesContent data-flx="messaging.inbox-popout.scheduled-messages-content" />
				</div>
			)}
		</div>
	);
	return (
		<FocusRingScope containerRef={containerRef} data-flx="messaging.inbox-popout.focus-ring-scope">
			<div
				className={clsx(styles.container, styles.containerWithSidebar)}
				ref={containerRef}
				style={{width: remFromPx(size.width), height: remFromPx(size.height)}}
				data-flx="messaging.inbox-popout.container"
			>
				<nav
					className={styles.sidebar}
					aria-label={i18n._(INBOX_TABS_DESCRIPTOR)}
					data-flx="messaging.inbox-popout.sidebar"
				>
					<div
						className={styles.sidebarTabList}
						role="tablist"
						aria-label={i18n._(INBOX_TABS_DESCRIPTOR)}
						aria-orientation="vertical"
						data-flx="messaging.inbox-popout.sidebar-tab-list"
					>
						{tabs.map((tab) => renderTabButton(tab))}
					</div>
					{normalizedActiveTab === 'mentions' && headerActions && (
						<div className={styles.sidebarFooterActions} data-flx="messaging.inbox-popout.sidebar-footer-actions">
							{headerActions}
						</div>
					)}
					{normalizedActiveTab === 'unreadChannels' && (
						<div className={styles.sidebarFooterActions} data-flx="messaging.inbox-popout.sidebar-footer-actions--2">
							<Tooltip
								text={i18n._(MARK_ALL_AS_READ_DESCRIPTOR)}
								position="right"
								data-flx="messaging.inbox-popout.tooltip"
							>
								<FocusRing offset={-2} data-flx="messaging.inbox-popout.focus-ring">
									<button
										type="button"
										className={styles.sidebarActionButton}
										onClick={handleMarkAllRead}
										disabled={unreadChannels.length === 0}
										aria-label={i18n._(MARK_ALL_INBOX_CHANNELS_AS_READ_DESCRIPTOR)}
										data-flx="messaging.inbox-popout.sidebar-action-button.mark-all-read"
									>
										<CheckIcon
											weight="bold"
											className={styles.iconSmall}
											data-flx="messaging.inbox-popout.icon-small--5"
										/>
									</button>
								</FocusRing>
							</Tooltip>
						</div>
					)}
				</nav>
				<div className={styles.mainPanel} data-flx="messaging.inbox-popout.main-panel">
					{content}
				</div>
				{INBOX_RESIZE_HANDLES.map(({edge, className, label}) => (
					<button
						key={edge}
						type="button"
						aria-label={i18n._(label)}
						aria-keyshortcuts="ArrowLeft ArrowRight ArrowUp ArrowDown Home End Enter Backspace Delete"
						className={clsx(styles.resizeCorner, className)}
						{...getHandleProps(edge)}
					/>
				))}
			</div>
		</FocusRingScope>
	);
});
