// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import {
	reportSkeletonChannelHeaderLayout,
	reportSkeletonChannelProjection,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {
	measureSkeletonTextWidthPx,
	measureSkeletonWidthPx,
	useSkeletonLayoutReport,
} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {useTextOverflow} from '@app/features/app/hooks/useTextOverflow';
import {ChannelDetailsBottomSheet} from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet';
import {ChannelSearchBottomSheet} from '@app/features/channel/components/bottomsheets/ChannelSearchBottomSheet';
import styles from '@app/features/channel/components/ChannelHeader.module.css';
import {CHANNEL_HEADER_DM_AVATAR_SIZE_PX} from '@app/features/channel/components/ChannelHeaderMetrics';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {
	ADD_FRIENDS_TO_GROUP_DESCRIPTOR,
	BACK_DESCRIPTOR,
	CHANNEL_ACTIONS_DESCRIPTOR,
	CREATE_GROUP_DM_DESCRIPTOR,
	EDIT_GROUP_DETAILS_DESCRIPTOR,
	HIDE_MEMBERS_DESCRIPTOR,
	MEMBERS_LIST_UNAVAILABLE_AT_THIS_SCREEN_WIDTH_DESCRIPTOR,
	OPEN_CHANNEL_DETAILS_FOR_DESCRIPTOR,
	OPEN_DIRECT_MESSAGE_DETAILS_FOR_DESCRIPTOR,
	OPEN_DIRECT_MESSAGE_PROFILE_DESCRIPTOR,
	OPEN_GROUP_DETAILS_FOR_DESCRIPTOR,
	OPEN_PROFILE_FOR_DESCRIPTOR,
	SEARCH_DESCRIPTOR,
	SHOW_CHANNEL_LIST_DESCRIPTOR,
	SHOW_MEMBERS_DESCRIPTOR,
	VIDEO_CALL_DESCRIPTOR,
} from '@app/features/channel/components/channel_header/shared';
import {useChannelHeaderData} from '@app/features/channel/components/channel_header/useChannelHeaderData';
import {CallButtons} from '@app/features/channel/components/channel_header_components/CallButtons';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {ChannelNotificationSettingsButton} from '@app/features/channel/components/channel_header_components/ChannelNotificationSettingsButton';
import {ChannelPinsButton} from '@app/features/channel/components/channel_header_components/ChannelPinsButton';
import {
	isUpdaterIconVisible,
	UpdaterIcon,
} from '@app/features/channel/components/channel_header_components/UpdaterIcon';
import {
	InboxButton,
	isStaffToolsButtonVisible,
	StaffToolsButton,
} from '@app/features/channel/components/channel_header_components/UtilityButtons';
import {useChannelSearchState} from '@app/features/channel/components/channel_view/useChannelSearchState';
import {MessageSearchBar} from '@app/features/channel/components/message_search_bar/MessageSearchBar';
import {ChannelTopicModal} from '@app/features/channel/components/modals/ChannelTopicModal';
import {CreateDMModal} from '@app/features/channel/components/modals/CreateDMModal';
import {EditGroupModal} from '@app/features/channel/components/modals/EditGroupModal';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {isGroupDmFull} from '@app/features/channel/utils/GroupDmUtils';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR,
	CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR,
	HIDE_FAVORITES_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {LexicalSearchInputHandle} from '@app/features/lexical/search/LexicalSearchInput';
import {useCanFitMemberList} from '@app/features/member/hooks/useMemberListVisible';
import MemberList from '@app/features/member/state/MemberList';
import * as FavoritesCommands from '@app/features/messaging/commands/FavoritesCommands';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import Favorites from '@app/features/messaging/state/Favorites';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {goBackOr} from '@app/features/platform/components/router/NavigationAdapter';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {AddFriendsToGroupModal} from '@app/features/relationship/components/modals/AddFriendsToGroupModal';
import Relationships from '@app/features/relationship/state/Relationships';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {ChannelContextMenu} from '@app/features/ui/action_menu/ChannelContextMenu';
import {DMContextMenu} from '@app/features/ui/action_menu/DMContextMenu';
import {GroupDMContextMenu} from '@app/features/ui/action_menu/GroupDMContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import ContextMenuState, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import PopoutState from '@app/features/ui/state/Popout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallState from '@app/features/voice/state/CallState';
import {computeChannelE2EEStatus} from '@app/features/voice/state/ChannelE2EEStatus';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowLeftIcon,
	CaretRightIcon,
	EyeSlashIcon,
	ListIcon,
	MagnifyingGlassIcon,
	PencilIcon,
	PhoneIcon,
	StarIcon,
	UserPlusIcon,
	UsersIcon,
	VideoCameraIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const {VoiceCallButton, VideoCallButton} = CallButtons;

interface ChannelHeaderProps {
	channel?: Channel;
	leftContent?: React.ReactNode;
	voiceCallHeaderSupplement?: React.ReactNode;
	showMembersToggle?: boolean;
	showPins?: boolean;
	onSearchSubmit?: (query: string, segments: Array<SearchSegment>) => void;
	onSearchClose?: () => void;
	isSearchResultsOpen?: boolean;
	forceVoiceCallStyle?: boolean;
	memberListDefaultHiddenForChannel?: boolean;
	onBackClick?: () => void;
	onVoiceCallChromePinChange?: (pinned: boolean) => void;
}

export const ChannelHeader = observer(
	({
		channel,
		leftContent,
		voiceCallHeaderSupplement = null,
		showMembersToggle = false,
		showPins = true,
		onSearchSubmit,
		onSearchClose,
		isSearchResultsOpen,
		forceVoiceCallStyle = false,
		memberListDefaultHiddenForChannel = false,
		onBackClick,
		onVoiceCallChromePinChange,
	}: ChannelHeaderProps) => {
		const {i18n} = useLingui();
		const location = useLocation();
		const headerRef = useRef<HTMLElement>(null);
		const {isMembersOpen} = MemberList;
		const isMobile = MobileLayout.isMobileLayout();
		const isCallChannelConnected = Boolean(MediaEngine.connected && MediaEngine.channelId === channel?.id);
		const channelIsE2EEEncrypted =
			isCallChannelConnected && channel
				? computeChannelE2EEStatus(channel.guildId ?? null, channel.id) === 'encrypted'
				: false;
		const e2eeIconOptions = channelIsE2EEEncrypted ? {e2eeEncrypted: true} : undefined;
		const isVoiceCallActive =
			!isMobile &&
			Boolean(
				channel &&
					(channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) &&
					isCallChannelConnected &&
					CallState.hasActiveCall(channel.id),
			);
		const isVoiceHeaderActive = isVoiceCallActive || forceVoiceCallStyle;
		const canFitMemberList = useCanFitMemberList();
		const memberListChannelId = channel?.id ?? null;
		const memberListUsesChannelOverride = Boolean(memberListDefaultHiddenForChannel && memberListChannelId);
		const isMembersToggleOpen = memberListUsesChannelOverride
			? MemberList.isDefaultHiddenChannelMembersOpen(memberListChannelId)
			: isMembersOpen;
		const [channelDetailsOpen, setChannelDetailsOpen] = useState(false);
		const [searchSheetOpen, setSearchSheetOpen] = useState(false);
		const [initialTab, setInitialTab] = useState<'members' | 'pins'>('members');
		const {
			searchQuery,
			searchSegments,
			isSearchActive,
			handleSearchInputChange: updateSearchInput,
			handleSearchSubmit: submitSearch,
			handleSearchClose: closeSearch,
		} = useChannelSearchState(channel);
		const latestSearchQueryRef = useRef('');
		const latestSearchSegmentsRef = useRef<Array<SearchSegment>>([]);
		const topicButtonRef = useRef<HTMLDivElement>(null);
		const [isTopicOverflowing, setIsTopicOverflowing] = useState(false);
		const contextMenuTarget = ContextMenuState.contextMenu?.target?.target ?? null;
		const isHeaderContextMenuOpen = Boolean(
			headerRef.current && isContextMenuNodeTarget(contextMenuTarget) && headerRef.current.contains(contextMenuTarget),
		);
		const isHeaderPopoutOpen = PopoutState.isOpen('inbox') || PopoutState.isOpen('channel-pins');
		const isVoiceCallChromePinned = isHeaderContextMenuOpen || isHeaderPopoutOpen;
		const channelId = channel?.id;
		const channelType = channel?.type;
		const channelHasTopic = Boolean(channel?.topic);
		const staffToolsVisible = isStaffToolsButtonVisible();
		const updaterVisible = isUpdaterIconVisible();
		const favoritesVisible = Accessibility.showFavorites;
		useSkeletonLayoutReport(
			() => reportSkeletonChannelHeaderLayout({staffToolsVisible, updaterVisible, favoritesVisible}),
			`${staffToolsVisible}|${updaterVisible}|${favoritesVisible}`,
		);
		useEffect(() => {
			latestSearchQueryRef.current = searchQuery;
			latestSearchSegmentsRef.current = searchSegments;
		}, [searchQuery, searchSegments]);
		useEffect(() => {
			onVoiceCallChromePinChange?.(isVoiceCallChromePinned);
		}, [isVoiceCallChromePinned, onVoiceCallChromePinChange]);
		useEffect(() => {
			return () => {
				onVoiceCallChromePinChange?.(false);
			};
		}, [onVoiceCallChromePinChange]);
		const searchInputRef = useRef<LexicalSearchInputHandle>(null);
		const isSearchResultsVisible = isSearchResultsOpen ?? isSearchActive;
		const dmNameRef = useRef<HTMLSpanElement>(null);
		const groupDMNameRef = useRef<HTMLSpanElement>(null);
		const guildChannelNameRef = useRef<HTMLSpanElement>(null);
		const isDMNameOverflowing = useTextOverflow(dmNameRef);
		const isGroupDMNameOverflowing = useTextOverflow(groupDMNameRef);
		const isGuildChannelNameOverflowing = useTextOverflow(guildChannelNameRef);
		const {isDM, isGroupDM, isPersonalNotes, isGuildChannel, recipient, directMessageName, groupDMName, channelName} =
			useChannelHeaderData(channel);
		const isBotDMRecipient = isDM && (recipient?.bot || recipient?.system);
		const isFavorited = channel && !isPersonalNotes ? !!Favorites.getChannel(channel.id) : false;
		const channelDetailsLabel =
			isDM && directMessageName
				? i18n._(OPEN_DIRECT_MESSAGE_DETAILS_FOR_DESCRIPTOR, {directMessageName})
				: isGroupDM && groupDMName
					? i18n._(OPEN_GROUP_DETAILS_FOR_DESCRIPTOR, {groupDMName})
					: i18n._(OPEN_CHANNEL_DETAILS_FOR_DESCRIPTOR, {channelName});
		const userProfileLabel = directMessageName
			? i18n._(OPEN_PROFILE_FOR_DESCRIPTOR, {directMessageName})
			: i18n._(OPEN_DIRECT_MESSAGE_PROFILE_DESCRIPTOR);
		const handleSearchInputChange = useCallback(
			(query: string, segments: Array<SearchSegment>) => {
				updateSearchInput(query, segments);
				latestSearchQueryRef.current = query;
				latestSearchSegmentsRef.current = segments;
			},
			[updateSearchInput],
		);
		const handleSearchSubmit = useCallback(() => {
			const query = latestSearchQueryRef.current;
			if (!query.trim()) {
				return;
			}
			if (onSearchSubmit) {
				onSearchSubmit(query, latestSearchSegmentsRef.current);
				return;
			}
			submitSearch(query, latestSearchSegmentsRef.current);
		}, [onSearchSubmit, submitSearch]);
		const handleSearchClose = useCallback(() => {
			updateSearchInput('', []);
			latestSearchQueryRef.current = '';
			latestSearchSegmentsRef.current = [];
			if (onSearchClose) {
				onSearchClose();
				return;
			}
			closeSearch();
		}, [updateSearchInput, onSearchClose, closeSearch]);
		const handleOpenCreateGroupDM = useCallback(() => {
			if (!channel) return;
			const initialRecipientIds = Array.from(channel.recipientIds);
			const excludeChannelId = channel.type === ChannelTypes.GROUP_DM ? channel.id : undefined;
			ModalCommands.push(
				modal(() => (
					<CreateDMModal
						initialSelectedUserIds={initialRecipientIds}
						duplicateExcludeChannelId={excludeChannelId}
						data-flx="channel.channel-header.handle-open-create-group-dm.create-dm-modal"
					/>
				)),
			);
		}, [channel]);
		const handleOpenEditGroup = useCallback(() => {
			if (!channel) return;
			ModalCommands.push(
				modal(() => (
					<EditGroupModal
						channelId={channel.id}
						data-flx="channel.channel-header.handle-open-edit-group.edit-group-modal"
					/>
				)),
			);
		}, [channel]);
		const handleOpenAddFriendsToGroup = useCallback(() => {
			if (!channel) return;
			ModalCommands.push(
				modal(() => (
					<AddFriendsToGroupModal
						channelId={channel.id}
						data-flx="channel.channel-header.handle-open-add-friends-to-group.add-friends-to-group-modal"
					/>
				)),
			);
		}, [channel]);
		const handleToggleMembers = useCallback(() => {
			if (!canFitMemberList) return;
			if (memberListUsesChannelOverride) {
				MemberList.toggleDefaultHiddenChannelMembers(memberListChannelId);
				return;
			}
			LayoutCommands.toggleMembers(!isMembersOpen);
		}, [isMembersOpen, canFitMemberList, memberListChannelId, memberListUsesChannelOverride]);
		useEffect(() => {
			const handleChannelDetailsOpen = (payload?: unknown) => {
				const {initialTab} = (payload ?? {}) as {initialTab?: 'members' | 'pins'};
				setInitialTab(initialTab || 'members');
				setChannelDetailsOpen(true);
			};
			return ComponentDispatch.subscribe('CHANNEL_DETAILS_OPEN', handleChannelDetailsOpen);
		}, []);
		useEffect(() => {
			if (!showMembersToggle) return;
			return ComponentDispatch.subscribe('CHANNEL_MEMBER_LIST_TOGGLE', () => {
				if (canFitMemberList) {
					if (memberListUsesChannelOverride) {
						MemberList.toggleDefaultHiddenChannelMembers(memberListChannelId);
						return;
					}
					LayoutCommands.toggleMembers(!isMembersOpen);
				}
			});
		}, [showMembersToggle, canFitMemberList, isMembersOpen, memberListChannelId, memberListUsesChannelOverride]);
		useEffect(() => {
			if (!channel?.topic) {
				setIsTopicOverflowing(false);
				return;
			}
			const el = topicButtonRef.current;
			if (!el) return;
			let rafId: number | null = null;
			const checkOverflow = () => {
				rafId = null;
				const {scrollWidth, clientWidth} = el;
				setIsTopicOverflowing(scrollWidth - clientWidth > 1);
			};
			const scheduleOverflowCheck = () => {
				if (rafId != null) return;
				rafId = requestAnimationFrame(checkOverflow);
			};
			scheduleOverflowCheck();
			const resizeObserver = new ResizeObserver(scheduleOverflowCheck);
			resizeObserver.observe(el);
			return () => {
				if (rafId != null) {
					cancelAnimationFrame(rafId);
				}
				resizeObserver.disconnect();
			};
		}, [channel?.topic]);
		const handleOpenUserProfile = useCallback(() => {
			if (!recipient) return;
			UserProfileCommands.openUserProfile(recipient.id);
		}, [recipient]);
		const handleBackClick = useCallback(() => {
			if (onBackClick) {
				onBackClick();
				return;
			}
			if (isDM || isGroupDM || isPersonalNotes) {
				RouterUtils.transitionTo(Routes.ME);
			} else if (Routes.isFavoritesRoute(location.pathname)) {
				RouterUtils.transitionTo(Routes.FAVORITES);
			} else if (isGuildChannel && channel?.guildId) {
				NavigationCommands.selectChannel(channel.guildId);
			} else {
				goBackOr(Routes.ME);
			}
		}, [onBackClick, isDM, isGroupDM, isPersonalNotes, isGuildChannel, channel?.guildId, location.pathname]);
		const handleChannelDetailsClick = () => {
			setInitialTab('members');
			setChannelDetailsOpen(true);
		};
		const handleOpenChannelTopic = useCallback(() => {
			if (!channel) return;
			ModalCommands.push(
				modal(() => <ChannelTopicModal channelId={channel.id} data-flx="channel.channel-header.channel-topic-modal" />),
			);
		}, [channel]);
		const handleSearchClick = () => {
			setSearchSheetOpen(true);
		};
		const handleContextMenu = useCallback(
			(event: React.MouseEvent) => {
				if (channel && isGuildChannel) {
					event.preventDefault();
					event.stopPropagation();
					ContextMenuCommands.openFromEvent(event, ({onClose}) => (
						<ChannelContextMenu
							channel={channel}
							onClose={onClose}
							data-flx="channel.channel-header.handle-context-menu.channel-context-menu"
						/>
					));
				}
			},
			[channel, isGuildChannel],
		);
		const handleUserContextMenu = useCallback(
			(event: React.MouseEvent) => {
				if (!channel) return;
				event.preventDefault();
				event.stopPropagation();
				if (isGroupDM) {
					ContextMenuCommands.openFromEvent(event, ({onClose}) => (
						<GroupDMContextMenu
							channel={channel}
							onClose={onClose}
							data-flx="channel.channel-header.handle-user-context-menu.group-dm-context-menu"
						/>
					));
				} else if (isDM && recipient) {
					ContextMenuCommands.openFromEvent(event, ({onClose}) => (
						<DMContextMenu
							channel={channel}
							recipient={recipient}
							onClose={onClose}
							data-flx="channel.channel-header.handle-user-context-menu.dm-context-menu"
						/>
					));
				}
			},
			[channel, isDM, isGroupDM, recipient],
		);
		const handleMobileVoiceCall = useCallback(
			async (event: React.MouseEvent) => {
				if (!channel) return;
				const isConnected = MediaEngine.connected;
				const connectedChannelId = MediaEngine.channelId;
				const isInCall = isConnected && connectedChannelId === channel.id;
				if (isInCall) {
					void CallCommands.leaveCall(channel.id);
				} else if (CallState.hasActiveCall(channel.id)) {
					CallCommands.joinCall(channel.id);
				} else {
					await CallUtils.requestStartCall(
						i18n,
						channel.id,
						CallUtils.getCallStartRequestOptions(event, {kind: 'voice'}),
					);
				}
			},
			[channel, i18n],
		);
		const handleMobileVideoCall = useCallback(
			async (event: React.MouseEvent) => {
				if (!channel) return;
				const isConnected = MediaEngine.connected;
				const connectedChannelId = MediaEngine.channelId;
				const isInCall = isConnected && connectedChannelId === channel.id;
				if (isInCall) {
					void CallCommands.leaveCall(channel.id);
				} else if (CallState.hasActiveCall(channel.id)) {
					CallCommands.joinCall(channel.id);
				} else {
					await CallUtils.requestStartCall(
						i18n,
						channel.id,
						CallUtils.getCallStartRequestOptions(event, {kind: 'video'}),
					);
				}
			},
			[channel, i18n],
		);
		const handleToggleFavorite = useCallback(() => {
			if (!channel || isPersonalNotes) return;
			if (isFavorited) {
				Favorites.removeChannel(channel.id);
				ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR)});
			} else {
				Favorites.addChannel(channel.id, channel.guildId ?? ME);
				ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR)});
			}
		}, [channel, isPersonalNotes, isFavorited]);
		const handleFavoriteContextMenu = useCallback(
			(event: React.MouseEvent) => {
				event.preventDefault();
				event.stopPropagation();
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<MenuGroup data-flx="channel.channel-header.handle-favorite-context-menu.menu-group">
						<MenuItem
							icon={<EyeSlashIcon data-flx="channel.channel-header.handle-favorite-context-menu.eye-slash-icon" />}
							onClick={() => {
								onClose();
								FavoritesCommands.confirmHideFavorites(undefined, i18n);
							}}
							danger
							data-flx="channel.channel-header.handle-favorite-context-menu.menu-item.close"
						>
							{i18n._(HIDE_FAVORITES_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				));
			},
			[i18n],
		);
		const isGroupDMFull = isGroupDmFull(channel);
		const isFriendDM =
			isDM &&
			recipient &&
			!isBotDMRecipient &&
			Relationships.getRelationship(recipient.id)?.type === RelationshipTypes.FRIEND;
		const shouldShowCreateGroupButton = !!channel && !isMobile && !isPersonalNotes && isFriendDM && !isGroupDM;
		const shouldShowAddFriendsButton = !!channel && !isMobile && !isPersonalNotes && isGroupDM && !isGroupDMFull;
		const hasChannel = Boolean(channel) && !isPersonalNotes;
		const hasCallableRecipients = hasChannel && (isDM || isGroupDM);
		let desktopLeadingActionCount = 0;
		if (hasChannel && isGuildChannel) {
			desktopLeadingActionCount += 1;
		}
		if (hasCallableRecipients && !(isDM && isBotDMRecipient)) {
			desktopLeadingActionCount += 2;
		}
		if (showPins && Boolean(channel)) {
			desktopLeadingActionCount += 1;
		}
		if (hasChannel && isFriendDM && !isGroupDM) {
			desktopLeadingActionCount += 1;
		}
		if (hasChannel && isGroupDM && !isGroupDMFull) {
			desktopLeadingActionCount += 1;
		}
		if (hasChannel && favoritesVisible) {
			desktopLeadingActionCount += 1;
		}
		if (showMembersToggle) {
			desktopLeadingActionCount += 1;
		}
		let mobileActionCount = 0;
		if (hasChannel && favoritesVisible) {
			mobileActionCount += 1;
		}
		if (hasCallableRecipients) {
			mobileActionCount += 2;
		}
		if (isGuildChannel) {
			mobileActionCount += 1;
		}
		const searchPanelOpen = Boolean(isSearchResultsVisible);
		useSkeletonLayoutReport(
			() => {
				if (channelId == null || channelType == null) {
					return;
				}
				const nameElement = dmNameRef.current ?? groupDMNameRef.current ?? guildChannelNameRef.current;
				reportSkeletonChannelProjection(channelId, channelType, {
					showTopic: channelHasTopic,
					nameWidthPx: measureSkeletonWidthPx(nameElement),
					topicWidthPx: measureSkeletonTextWidthPx(topicButtonRef.current),
					desktopLeadingActionCount,
					mobileActionCount,
					memberListVisible: isMembersToggleOpen,
					searchPanelOpen,
				});
			},
			`${channelId}|${channelType ?? ''}|${channelHasTopic}|${channel?.topic ?? ''}|${channelName}|${directMessageName}|${groupDMName}|${desktopLeadingActionCount}|${mobileActionCount}|${searchPanelOpen}|${isMembersToggleOpen}|${Accessibility.fontSize}|${Accessibility.zoomLevel}`,
		);
		return (
			<>
				<header
					ref={headerRef}
					className={clsx(styles.headerWrapper, isVoiceHeaderActive && styles.headerWrapperCallActive)}
					data-voice-call-channel-header={isVoiceHeaderActive ? 'true' : undefined}
					data-flx="channel.channel-header.header-wrapper"
				>
					<NativeDragRegion
						className={clsx(styles.headerContainer, isVoiceHeaderActive && styles.headerContainerCallActive)}
						data-flx="channel.channel-header.header-container"
					>
						<div className={styles.headerLeftSection} data-flx="channel.channel-header.header-left-section">
							{isMobile ? (
								<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring">
									<button
										type="button"
										className={styles.backButton}
										aria-label={i18n._(BACK_DESCRIPTOR)}
										onClick={handleBackClick}
										data-flx="channel.channel-header.back-button.back-click"
									>
										<ArrowLeftIcon
											className={styles.backIconBold}
											weight="bold"
											data-flx="channel.channel-header.back-icon-bold"
										/>
									</button>
								</FocusRing>
							) : (
								<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--2">
									<button
										type="button"
										className={styles.backButtonDesktop}
										aria-label={i18n._(SHOW_CHANNEL_LIST_DESCRIPTOR)}
										onClick={handleBackClick}
										data-flx="channel.channel-header.back-button-desktop.back-click"
									>
										<ListIcon className={styles.backIcon} data-flx="channel.channel-header.back-icon" />
									</button>
								</FocusRing>
							)}
							<div className={styles.leftContentContainer} data-flx="channel.channel-header.left-content-container">
								{leftContent ? (
									leftContent
								) : channel ? (
									isMobile ? (
										<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--3">
											<button
												type="button"
												className={styles.mobileButton}
												aria-label={channelDetailsLabel}
												aria-haspopup="dialog"
												onClick={handleChannelDetailsClick}
												data-flx="channel.channel-header.mobile-button.channel-details-click"
											>
												{isDM && recipient ? (
													<>
														<StatusAwareAvatar
															user={recipient}
															size={CHANNEL_HEADER_DM_AVATAR_SIZE_PX}
															showOffline={true}
															data-flx="channel.channel-header.status-aware-avatar"
														/>
														<span className={styles.dmNameWrapper} data-flx="channel.channel-header.dm-name-wrapper">
															<Tooltip
																text={isDMNameOverflowing && directMessageName ? directMessageName : ''}
																data-flx="channel.channel-header.tooltip"
															>
																<span
																	ref={dmNameRef}
																	className={styles.channelName}
																	data-flx="channel.channel-header.channel-name"
																>
																	{directMessageName}
																</span>
															</Tooltip>
															{isBotDMRecipient && (
																<UserTag
																	className={styles.userTag}
																	system={recipient.system}
																	data-flx="channel.channel-header.user-tag"
																/>
															)}
														</span>
														<CaretRightIcon
															className={styles.caretRight}
															weight="bold"
															data-flx="channel.channel-header.caret-right"
														/>
													</>
												) : isGroupDM ? (
													<>
														<GroupDMAvatar
															channel={channel}
															size={CHANNEL_HEADER_DM_AVATAR_SIZE_PX}
															data-flx="channel.channel-header.group-dm-avatar"
														/>
														<Tooltip
															text={isGroupDMNameOverflowing && groupDMName ? groupDMName : ''}
															data-flx="channel.channel-header.tooltip--2"
														>
															<span
																ref={groupDMNameRef}
																className={styles.channelName}
																data-flx="channel.channel-header.channel-name--2"
															>
																{groupDMName}
															</span>
														</Tooltip>
														<CaretRightIcon
															className={styles.caretRight}
															weight="bold"
															data-flx="channel.channel-header.caret-right--2"
														/>
													</>
												) : (
													<>
														{ChannelUtils.getIcon(channel, {className: styles.channelIcon}, e2eeIconOptions)}
														<Tooltip
															text={isGuildChannelNameOverflowing && channelName ? channelName : ''}
															data-flx="channel.channel-header.tooltip--3"
														>
															<span
																ref={guildChannelNameRef}
																className={styles.channelName}
																data-flx="channel.channel-header.channel-name--3"
															>
																{channelName}
															</span>
														</Tooltip>
														<CaretRightIcon
															className={styles.caretRight}
															weight="bold"
															data-flx="channel.channel-header.caret-right--3"
														/>
													</>
												)}
											</button>
										</FocusRing>
									) : isDM && recipient ? (
										<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--4">
											<button
												type="button"
												className={styles.desktopButton}
												aria-label={userProfileLabel}
												onClick={handleOpenUserProfile}
												onContextMenu={handleUserContextMenu}
												data-flx="channel.channel-header.desktop-button.open-user-profile"
											>
												<StatusAwareAvatar
													user={recipient}
													size={CHANNEL_HEADER_DM_AVATAR_SIZE_PX}
													showOffline={true}
													data-flx="channel.channel-header.status-aware-avatar--2"
												/>
												<span className={styles.dmNameWrapper} data-flx="channel.channel-header.dm-name-wrapper--2">
													<Tooltip
														text={isDMNameOverflowing ? directMessageName : ''}
														data-flx="channel.channel-header.tooltip--4"
													>
														<span
															ref={dmNameRef}
															className={styles.channelName}
															data-flx="channel.channel-header.channel-name--4"
														>
															{directMessageName}
														</span>
													</Tooltip>
													{isBotDMRecipient && (
														<UserTag
															className={styles.userTag}
															system={recipient.system}
															data-flx="channel.channel-header.user-tag--2"
														/>
													)}
												</span>
											</button>
										</FocusRing>
									) : isGroupDM ? (
										isMobile ? (
											<div className={styles.avatarWrapper} data-flx="channel.channel-header.avatar-wrapper">
												<GroupDMAvatar
													channel={channel}
													size={CHANNEL_HEADER_DM_AVATAR_SIZE_PX}
													data-flx="channel.channel-header.group-dm-avatar--2"
												/>
												<Tooltip
													text={isGroupDMNameOverflowing && groupDMName ? groupDMName : ''}
													data-flx="channel.channel-header.tooltip--5"
												>
													<span
														ref={groupDMNameRef}
														className={styles.channelName}
														data-flx="channel.channel-header.channel-name--5"
													>
														{groupDMName}
													</span>
												</Tooltip>
											</div>
										) : (
											<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--5">
												<div
													className={styles.groupDmHeaderTrigger}
													role="button"
													aria-label={i18n._(EDIT_GROUP_DETAILS_DESCRIPTOR)}
													tabIndex={0}
													onClick={handleOpenEditGroup}
													onContextMenu={handleUserContextMenu}
													onKeyDown={(event) => {
														if (isKeyboardActivationKey(event.key)) {
															event.preventDefault();
															handleOpenEditGroup();
														}
													}}
													data-flx="channel.channel-header.group-dm-header-trigger.open-edit-group"
												>
													<div
														className={styles.groupDmHeaderInner}
														data-flx="channel.channel-header.group-dm-header-inner"
													>
														<GroupDMAvatar
															channel={channel}
															size={CHANNEL_HEADER_DM_AVATAR_SIZE_PX}
															data-flx="channel.channel-header.group-dm-avatar--3"
														/>
														<div className={styles.dmNameWrapper} data-flx="channel.channel-header.dm-name-wrapper--3">
															<Tooltip
																text={isGroupDMNameOverflowing && groupDMName ? groupDMName : ''}
																data-flx="channel.channel-header.tooltip--6"
															>
																<span
																	ref={groupDMNameRef}
																	className={clsx(styles.channelName, styles.groupDmChannelName)}
																	data-flx="channel.channel-header.channel-name--6"
																>
																	{groupDMName}
																</span>
															</Tooltip>
														</div>
													</div>
													<PencilIcon
														className={styles.groupDmEditIcon}
														size={16}
														weight="bold"
														data-flx="channel.channel-header.group-dm-edit-icon"
													/>
												</div>
											</FocusRing>
										)
									) : isPersonalNotes ? (
										<div className={styles.avatarWrapper} data-flx="channel.channel-header.avatar-wrapper--2">
											{ChannelUtils.getIcon(channel, {className: styles.channelIcon}, e2eeIconOptions)}
											<Tooltip
												text={isGuildChannelNameOverflowing && channelName ? channelName : ''}
												data-flx="channel.channel-header.tooltip--7"
											>
												<span
													ref={guildChannelNameRef}
													className={styles.channelName}
													data-flx="channel.channel-header.channel-name--7"
												>
													{channelName}
												</span>
											</Tooltip>
										</div>
									) : (
										<div
											role="group"
											className={styles.channelInfoContainer}
											onContextMenu={handleContextMenu}
											data-flx="channel.channel-header.channel-info-container.context-menu"
										>
											{ChannelUtils.getIcon(channel, {className: styles.channelIcon}, e2eeIconOptions)}
											<Tooltip
												text={isGuildChannelNameOverflowing && channelName ? channelName : ''}
												data-flx="channel.channel-header.tooltip--8"
											>
												<span
													ref={guildChannelNameRef}
													className={styles.channelName}
													data-flx="channel.channel-header.channel-name--8"
												>
													{channelName}
												</span>
											</Tooltip>
											{channel.topic && (
												<>
													<span className={styles.topicDivider} data-flx="channel.channel-header.topic-divider">
														•
													</span>
													<div className={styles.topicContainer} data-flx="channel.channel-header.topic-container">
														<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--6">
															<div
																role="button"
																ref={topicButtonRef}
																className={clsx(
																	markupStyles.markup,
																	styles.topicButton,
																	isTopicOverflowing && styles.topicButtonOverflow,
																)}
																onClick={handleOpenChannelTopic}
																onKeyDown={(e) => {
																	if (!isKeyboardActivationKey(e.key)) return;
																	e.preventDefault();
																	handleOpenChannelTopic();
																}}
																tabIndex={0}
																data-flx="channel.channel-header.topic-button.push"
															>
																<SafeMarkdown
																	content={channel.topic}
																	options={{
																		context: MarkdownContext.RESTRICTED_INLINE_REPLY,
																		disableInteractions: true,
																		channelId: channel.id,
																	}}
																	data-flx="channel.channel-header.safe-markdown"
																/>
															</div>
														</FocusRing>
													</div>
												</>
											)}
										</div>
									)
								) : null}
							</div>
						</div>
						<div
							className={styles.headerRightSection}
							role="group"
							aria-label={i18n._(CHANNEL_ACTIONS_DESCRIPTOR)}
							data-flx="channel.channel-header.header-right-section"
						>
							{voiceCallHeaderSupplement && (
								<div
									className={styles.voiceCallHeaderSupplement}
									data-flx="channel.channel-header.voice-call-header-supplement"
								>
									{voiceCallHeaderSupplement}
								</div>
							)}
							{isMobile && channel && !isPersonalNotes && Accessibility.showFavorites && (
								<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--7">
									<button
										type="button"
										className={styles.iconButtonMobile}
										aria-label={
											isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)
										}
										aria-pressed={isFavorited}
										onClick={handleToggleFavorite}
										onContextMenu={handleFavoriteContextMenu}
										data-flx="channel.channel-header.icon-button-mobile.toggle-favorite"
									>
										<StarIcon
											className={styles.buttonIconMobile}
											weight={isFavorited ? 'fill' : 'bold'}
											data-flx="channel.channel-header.button-icon-mobile"
										/>
									</button>
								</FocusRing>
							)}
							{isMobile && (isDM || isGroupDM) && !isPersonalNotes && (
								<>
									<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--8">
										<button
											type="button"
											className={styles.iconButtonMobile}
											aria-label={i18n._(VOICE_CALL_DESCRIPTOR)}
											onClick={handleMobileVoiceCall}
											data-flx="channel.channel-header.icon-button-mobile.mobile-voice-call"
										>
											<PhoneIcon
												className={styles.buttonIconMobile}
												data-flx="channel.channel-header.button-icon-mobile--2"
											/>
										</button>
									</FocusRing>
									<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--9">
										<button
											type="button"
											className={styles.iconButtonMobile}
											aria-label={i18n._(VIDEO_CALL_DESCRIPTOR)}
											onClick={handleMobileVideoCall}
											data-flx="channel.channel-header.icon-button-mobile.mobile-video-call"
										>
											<VideoCameraIcon
												className={styles.buttonIconMobile}
												data-flx="channel.channel-header.button-icon-mobile--3"
											/>
										</button>
									</FocusRing>
								</>
							)}
							{isMobile && isGuildChannel && (
								<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--10">
									<button
										type="button"
										className={styles.iconButtonMobile}
										aria-label={i18n._(SEARCH_DESCRIPTOR)}
										onClick={handleSearchClick}
										data-flx="channel.channel-header.icon-button-mobile.search-click"
									>
										<MagnifyingGlassIcon
											className={styles.buttonIconMobile}
											weight="bold"
											data-flx="channel.channel-header.button-icon-mobile--4"
										/>
									</button>
								</FocusRing>
							)}
							{channel && isGuildChannel && !isMobile && !isPersonalNotes && (
								<ChannelNotificationSettingsButton
									channel={channel}
									data-flx="channel.channel-header.channel-notification-settings-button"
								/>
							)}
							{(isDM || isGroupDM) && channel && !isMobile && !(isDM && isBotDMRecipient) && (
								<>
									<VoiceCallButton channel={channel} data-flx="channel.channel-header.voice-call-button" />
									<VideoCallButton channel={channel} data-flx="channel.channel-header.video-call-button" />
								</>
							)}
							{showPins && channel && !isMobile && (
								<ChannelPinsButton channel={channel} data-flx="channel.channel-header.channel-pins-button" />
							)}
							{shouldShowCreateGroupButton && (
								<ChannelHeaderIcon
									icon={UserPlusIcon}
									label={i18n._(CREATE_GROUP_DM_DESCRIPTOR)}
									onClick={handleOpenCreateGroupDM}
									data-flx="channel.channel-header.channel-header-icon.open-create-group-dm"
								/>
							)}
							{shouldShowAddFriendsButton && (
								<ChannelHeaderIcon
									icon={UserPlusIcon}
									label={i18n._(ADD_FRIENDS_TO_GROUP_DESCRIPTOR)}
									onClick={handleOpenAddFriendsToGroup}
									data-flx="channel.channel-header.channel-header-icon.open-add-friends-to-group"
								/>
							)}
							{channel && !isMobile && !isPersonalNotes && Accessibility.showFavorites && (
								<Tooltip
									text={isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)}
									position="bottom"
									data-flx="channel.channel-header.tooltip--9"
								>
									<FocusRing offset={-2} data-flx="channel.channel-header.focus-ring--11">
										<button
											type="button"
											className={isFavorited ? styles.iconButtonSelected : styles.iconButtonDefault}
											aria-label={
												isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)
											}
											aria-pressed={isFavorited}
											onClick={handleToggleFavorite}
											onContextMenu={handleFavoriteContextMenu}
											data-flx="channel.channel-header.icon-button.toggle-favorite"
										>
											<StarIcon
												className={styles.buttonIcon}
												weight={isFavorited ? 'fill' : 'bold'}
												data-flx="channel.channel-header.button-icon"
											/>
										</button>
									</FocusRing>
								</Tooltip>
							)}
							{showMembersToggle && !isMobile && (
								<ChannelHeaderIcon
									icon={UsersIcon}
									isSelected={isMembersToggleOpen}
									label={
										!canFitMemberList
											? i18n._(MEMBERS_LIST_UNAVAILABLE_AT_THIS_SCREEN_WIDTH_DESCRIPTOR)
											: isMembersToggleOpen
												? i18n._(HIDE_MEMBERS_DESCRIPTOR)
												: i18n._(SHOW_MEMBERS_DESCRIPTOR)
									}
									onClick={handleToggleMembers}
									disabled={!canFitMemberList}
									aria-pressed={isMembersToggleOpen}
									keybindAction="chat_toggle_member_list"
									data-flx="channel.channel-header.channel-header-icon.toggle-members"
								/>
							)}
							{!isMobile && channel && !isPersonalNotes && (
								<FocusRing offset={-2} within data-flx="channel.channel-header.focus-ring--12">
									<div
										className={styles.messageSearchFocusWrapper}
										data-flx="channel.channel-header.message-search-focus-wrapper"
									>
										<MessageSearchBar
											channel={channel}
											value={searchQuery}
											onChange={handleSearchInputChange}
											onSearch={handleSearchSubmit}
											onClear={handleSearchClose}
											isResultsOpen={Boolean(isSearchResultsVisible)}
											onCloseResults={handleSearchClose}
											inputRefExternal={searchInputRef}
											highContrast={isVoiceHeaderActive}
											data-flx="channel.channel-header.message-search-bar.search-input-change"
										/>
									</div>
								</FocusRing>
							)}
							{!isMobile && <UpdaterIcon data-flx="channel.channel-header.updater-icon" />}
							{!isMobile && <StaffToolsButton data-flx="channel.channel-header.staff-tools-button" />}
							{!isMobile && <InboxButton data-flx="channel.channel-header.inbox-button" />}
						</div>
					</NativeDragRegion>
				</header>
				{channel && (
					<>
						<ChannelDetailsBottomSheet
							isOpen={channelDetailsOpen}
							onClose={() => {
								setChannelDetailsOpen(false);
								setInitialTab('members');
							}}
							channel={channel}
							initialTab={initialTab}
							data-flx="channel.channel-header.channel-details-bottom-sheet"
						/>
						<ChannelSearchBottomSheet
							isOpen={searchSheetOpen}
							onClose={() => setSearchSheetOpen(false)}
							channel={channel}
							data-flx="channel.channel-header.channel-search-bottom-sheet"
						/>
					</>
				)}
			</>
		);
	},
);
