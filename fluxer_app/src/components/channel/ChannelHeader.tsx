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
import React from 'react';
import * as CallActionCreators from '~/actions/CallActionCreators';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as FavoritesActionCreators from '~/actions/FavoritesActionCreators';
import * as LayoutActionCreators from '~/actions/LayoutActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {ChannelTypes, ME, RelationshipTypes} from '~/Constants';
import {ChannelDetailsBottomSheet} from '~/components/bottomsheets/ChannelDetailsBottomSheet';
import {MessageSearchBar} from '~/components/channel/MessageSearchBar';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {NativeDragRegion} from '~/components/layout/NativeDragRegion';
import {AddFriendsToGroupModal} from '~/components/modals/AddFriendsToGroupModal';
import {ChannelTopicModal} from '~/components/modals/ChannelTopicModal';
import {CreateDMModal} from '~/components/modals/CreateDMModal';
import {EditGroupModal} from '~/components/modals/EditGroupModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {useCanFitMemberList} from '~/hooks/useMemberListVisible';
import {useTextOverflow} from '~/hooks/useTextOverflow';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import {useLocation} from '~/lib/router/react';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import CallStateStore from '~/stores/CallStateStore';
import FavoritesStore from '~/stores/FavoritesStore';
import MemberListStore from '~/stores/MemberListStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import RelationshipStore from '~/stores/RelationshipStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import markupStyles from '~/styles/Markup.module.css';
import * as CallUtils from '~/utils/CallUtils';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {MAX_GROUP_DM_RECIPIENTS} from '~/utils/groupDmUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import type {SearchSegment} from '~/utils/SearchSegmentManager';
import {UserTag} from '../channel/UserTag';
import {ChannelContextMenu} from '../uikit/ContextMenu/ChannelContextMenu';
import {MenuGroup} from '../uikit/ContextMenu/MenuGroup';
import {MenuItem} from '../uikit/ContextMenu/MenuItem';
import {Tooltip} from '../uikit/Tooltip/Tooltip';
import {CallButtons} from './ChannelHeader/CallButtons';
import {ChannelHeaderIcon} from './ChannelHeader/ChannelHeaderIcon';
import {ChannelNotificationSettingsButton} from './ChannelHeader/ChannelNotificationSettingsButton';
import {ChannelPinsButton} from './ChannelHeader/ChannelPinsButton';
import {UpdaterIcon} from './ChannelHeader/UpdaterIcon';
import {InboxButton} from './ChannelHeader/UtilityButtons';
import styles from './ChannelHeader.module.css';
import {useChannelHeaderData} from './channel-header/useChannelHeaderData';

const {VoiceCallButton, VideoCallButton} = CallButtons;

interface ChannelHeaderProps {
	channel?: ChannelRecord;
	leftContent?: React.ReactNode;
	showMembersToggle?: boolean;
	showPins?: boolean;
	onSearchSubmit?: (query: string, segments: Array<SearchSegment>) => void;
	onSearchClose?: () => void;
	isSearchResultsOpen?: boolean;
	forceVoiceCallStyle?: boolean;
}

export const ChannelHeader = observer(
	({
		channel,
		leftContent,
		showMembersToggle = false,
		showPins = true,
		onSearchSubmit,
		onSearchClose,
		isSearchResultsOpen,
		forceVoiceCallStyle = false,
	}: ChannelHeaderProps) => {
		const {t, i18n} = useLingui();

		const location = useLocation();
		const {isMembersOpen} = MemberListStore;
		const isMobile = MobileLayoutStore.isMobileLayout();
		const isCallChannelConnected = Boolean(MediaEngineStore.connected && MediaEngineStore.channelId === channel?.id);
		const isVoiceCallActive =
			!isMobile &&
			Boolean(
				channel &&
					(channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) &&
					isCallChannelConnected &&
					CallStateStore.hasActiveCall(channel.id),
			);
		const isVoiceHeaderActive = isVoiceCallActive || forceVoiceCallStyle;
		const canFitMemberList = useCanFitMemberList();

		const [channelDetailsOpen, setChannelDetailsOpen] = React.useState(false);
		const [openSearchImmediately, setOpenSearchImmediately] = React.useState(false);
		const [initialTab, setInitialTab] = React.useState<'members' | 'pins'>('members');
		const [searchQuery, setSearchQuery] = React.useState('');
		const [searchSegments, setSearchSegments] = React.useState<Array<SearchSegment>>([]);
		const latestSearchQueryRef = React.useRef('');
		const latestSearchSegmentsRef = React.useRef<Array<SearchSegment>>([]);
		const topicButtonRef = React.useRef<HTMLDivElement>(null);
		const [isTopicOverflowing, setIsTopicOverflowing] = React.useState(false);
		React.useEffect(() => {
			latestSearchQueryRef.current = searchQuery;
			latestSearchSegmentsRef.current = searchSegments;
		}, [searchQuery, searchSegments]);
		const searchInputRef = React.useRef<HTMLInputElement>(null);

		const dmNameRef = React.useRef<HTMLSpanElement>(null);
		const groupDMNameRef = React.useRef<HTMLSpanElement>(null);
		const guildChannelNameRef = React.useRef<HTMLSpanElement>(null);

		const isDMNameOverflowing = useTextOverflow(dmNameRef);
		const isGroupDMNameOverflowing = useTextOverflow(groupDMNameRef);
		const isGuildChannelNameOverflowing = useTextOverflow(guildChannelNameRef);

		const {
			isDM,
			isGroupDM,
			isPersonalNotes,
			isGuildChannel,
			isVoiceChannel,
			recipient,
			directMessageName,
			groupDMName,
			channelName,
		} = useChannelHeaderData(channel);
		const isBotDMRecipient = isDM && recipient?.bot;

		const isFavorited = channel && !isPersonalNotes ? !!FavoritesStore.getChannel(channel.id) : false;

		const handleOpenCreateGroupDM = React.useCallback(() => {
			if (!channel) return;
			const initialRecipientIds = Array.from(channel.recipientIds);
			const excludeChannelId = channel.type === ChannelTypes.GROUP_DM ? channel.id : undefined;
			ModalActionCreators.push(
				modal(() => (
					<CreateDMModal initialSelectedUserIds={initialRecipientIds} duplicateExcludeChannelId={excludeChannelId} />
				)),
			);
		}, [channel]);

		const handleOpenEditGroup = React.useCallback(() => {
			if (!channel) return;
			ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
		}, [channel]);
		const handleOpenAddFriendsToGroup = React.useCallback(() => {
			if (!channel) return;
			ModalActionCreators.push(modal(() => <AddFriendsToGroupModal channelId={channel.id} />));
		}, [channel]);

		const handleToggleMembers = React.useCallback(() => {
			if (!canFitMemberList) return;
			LayoutActionCreators.toggleMembers(!isMembersOpen);
		}, [isMembersOpen, canFitMemberList]);

		React.useEffect(() => {
			const handleChannelDetailsOpen = (payload?: unknown) => {
				const {initialTab} = (payload ?? {}) as {initialTab?: 'members' | 'pins'};
				setInitialTab(initialTab || 'members');
				setOpenSearchImmediately(false);
				setChannelDetailsOpen(true);
			};

			return ComponentDispatch.subscribe('CHANNEL_DETAILS_OPEN', handleChannelDetailsOpen);
		}, []);

		React.useEffect(() => {
			if (!showMembersToggle) return;
			return ComponentDispatch.subscribe('CHANNEL_MEMBER_LIST_TOGGLE', () => {
				if (canFitMemberList) {
					LayoutActionCreators.toggleMembers(!isMembersOpen);
				}
			});
		}, [showMembersToggle, canFitMemberList, isMembersOpen]);

		React.useEffect(() => {
			if (!channel?.topic) {
				setIsTopicOverflowing(false);
				return;
			}

			const el = topicButtonRef.current;
			if (!el) return;

			const checkOverflow = () => {
				const {scrollWidth, clientWidth} = el;
				setIsTopicOverflowing(scrollWidth - clientWidth > 1);
			};

			checkOverflow();

			const resizeObserver = new ResizeObserver(checkOverflow);
			resizeObserver.observe(el);

			return () => {
				resizeObserver.disconnect();
			};
		}, [channel?.topic]);

		const handleOpenUserProfile = React.useCallback(() => {
			if (!recipient) return;
			UserProfileActionCreators.openUserProfile(recipient.id);
		}, [recipient]);

		const handleBackClick = React.useCallback(() => {
			if (isDM || isGroupDM || isPersonalNotes) {
				RouterUtils.transitionTo(Routes.ME);
			} else if (Routes.isFavoritesRoute(location.pathname)) {
				RouterUtils.transitionTo(Routes.FAVORITES);
			} else if (isGuildChannel && channel?.guildId) {
				RouterUtils.transitionTo(Routes.guildChannel(channel.guildId));
			} else {
				window.history.back();
			}
		}, [isDM, isGroupDM, isPersonalNotes, isGuildChannel, channel?.guildId, location.pathname]);

		const handleChannelDetailsClick = () => {
			setInitialTab('members');
			setOpenSearchImmediately(false);
			setChannelDetailsOpen(true);
		};

		const handleSearchClick = () => {
			setInitialTab('members');
			setOpenSearchImmediately(true);
			setChannelDetailsOpen(true);
		};

		const handleContextMenu = React.useCallback(
			(event: React.MouseEvent) => {
				if (channel && isGuildChannel) {
					event.preventDefault();
					event.stopPropagation();
					ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
						<ChannelContextMenu channel={channel} onClose={onClose} />
					));
				}
			},
			[channel, isGuildChannel],
		);

		const handleMobileVoiceCall = React.useCallback(
			async (event: React.MouseEvent) => {
				if (!channel) return;
				const isConnected = MediaEngineStore.connected;
				const connectedChannelId = MediaEngineStore.channelId;
				const isInCall = isConnected && connectedChannelId === channel.id;

				if (isInCall) {
					void CallActionCreators.leaveCall(channel.id);
				} else if (CallStateStore.hasActiveCall(channel.id)) {
					CallActionCreators.joinCall(channel.id);
				} else {
					const silent = event.shiftKey;
					await CallUtils.checkAndStartCall(channel.id, silent);
				}
			},
			[channel],
		);

		const handleMobileVideoCall = React.useCallback(
			async (event: React.MouseEvent) => {
				if (!channel) return;
				const isConnected = MediaEngineStore.connected;
				const connectedChannelId = MediaEngineStore.channelId;
				const isInCall = isConnected && connectedChannelId === channel.id;

				if (isInCall) {
					void CallActionCreators.leaveCall(channel.id);
				} else if (CallStateStore.hasActiveCall(channel.id)) {
					CallActionCreators.joinCall(channel.id);
				} else {
					const silent = event.shiftKey;
					await CallUtils.checkAndStartCall(channel.id, silent);
				}
			},
			[channel],
		);

		const handleToggleFavorite = React.useCallback(() => {
			if (!channel || isPersonalNotes) return;

			if (isFavorited) {
				FavoritesStore.removeChannel(channel.id);
				ToastActionCreators.createToast({type: 'success', children: t`Channel removed from favorites`});
			} else {
				FavoritesStore.addChannel(channel.id, channel.guildId ?? ME);
				ToastActionCreators.createToast({type: 'success', children: t`Channel added to favorites`});
			}
		}, [channel, isPersonalNotes, isFavorited]);

		const handleFavoriteContextMenu = React.useCallback(
			(event: React.MouseEvent) => {
				event.preventDefault();
				event.stopPropagation();

				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<MenuGroup>
						<MenuItem
							icon={<EyeSlashIcon />}
							onClick={() => {
								onClose();
								FavoritesActionCreators.confirmHideFavorites(undefined, i18n);
							}}
							danger
						>
							{t`Hide Favorites`}
						</MenuItem>
					</MenuGroup>
				));
			},
			[t],
		);

		const isGroupDMFull = channel ? channel.recipientIds.length + 1 >= MAX_GROUP_DM_RECIPIENTS : false;
		const isFriendDM =
			isDM &&
			recipient &&
			!isBotDMRecipient &&
			RelationshipStore.getRelationship(recipient.id)?.type === RelationshipTypes.FRIEND;
		const shouldShowCreateGroupButton = !!channel && !isMobile && !isPersonalNotes && isFriendDM && !isGroupDM;
		const shouldShowAddFriendsButton = !!channel && !isMobile && !isPersonalNotes && isGroupDM && !isGroupDMFull;

		return (
			<>
				<div className={clsx(styles.headerWrapper, isVoiceHeaderActive && styles.headerWrapperCallActive)}>
					<NativeDragRegion
						className={clsx(styles.headerContainer, isVoiceHeaderActive && styles.headerContainerCallActive)}
					>
						<div className={styles.headerLeftSection}>
							{isMobile ? (
								<FocusRing offset={-2}>
									<button type="button" className={styles.backButton} onClick={handleBackClick}>
										<ArrowLeftIcon className={styles.backIconBold} weight="bold" />
									</button>
								</FocusRing>
							) : (
								<FocusRing offset={-2}>
									<button type="button" className={styles.backButtonDesktop} onClick={handleBackClick}>
										<ListIcon className={styles.backIcon} />
									</button>
								</FocusRing>
							)}

							<div className={styles.leftContentContainer}>
								{leftContent ? (
									leftContent
								) : channel ? (
									isMobile ? (
										<FocusRing offset={-2}>
											<button type="button" className={styles.mobileButton} onClick={handleChannelDetailsClick}>
												{isDM && recipient ? (
													<>
														<StatusAwareAvatar user={recipient} size={32} showOffline={true} />
														<span className={styles.dmNameWrapper}>
															<Tooltip text={isDMNameOverflowing && directMessageName ? directMessageName : ''}>
																<span ref={dmNameRef} className={styles.channelName}>
																	{directMessageName}
																</span>
															</Tooltip>
															{isBotDMRecipient && <UserTag className={styles.userTag} system={recipient.system} />}
														</span>
														<CaretRightIcon className={styles.caretRight} weight="bold" />
													</>
												) : isGroupDM ? (
													<>
														<GroupDMAvatar channel={channel} size={32} />
														<Tooltip text={isGroupDMNameOverflowing && groupDMName ? groupDMName : ''}>
															<span ref={groupDMNameRef} className={styles.channelName}>
																{groupDMName}
															</span>
														</Tooltip>
														<CaretRightIcon className={styles.caretRight} weight="bold" />
													</>
												) : (
													<>
														{ChannelUtils.getIcon(channel, {className: styles.channelIcon})}
														<Tooltip text={isGuildChannelNameOverflowing && channelName ? channelName : ''}>
															<span ref={guildChannelNameRef} className={styles.channelName}>
																{channelName}
															</span>
														</Tooltip>
														<CaretRightIcon className={styles.caretRight} weight="bold" />
													</>
												)}
											</button>
										</FocusRing>
									) : isDM && recipient ? (
										<FocusRing offset={-2}>
											<button type="button" className={styles.desktopButton} onClick={handleOpenUserProfile}>
												<StatusAwareAvatar user={recipient} size={32} showOffline={true} />
												<span className={styles.dmNameWrapper}>
													<Tooltip text={isDMNameOverflowing ? directMessageName : ''}>
														<span ref={dmNameRef} className={styles.channelName}>
															{directMessageName}
														</span>
													</Tooltip>
													{isBotDMRecipient && <UserTag className={styles.userTag} system={recipient.system} />}
												</span>
											</button>
										</FocusRing>
									) : isGroupDM ? (
										isMobile ? (
											<div className={styles.avatarWrapper}>
												<GroupDMAvatar channel={channel} size={32} />
												<Tooltip text={isGroupDMNameOverflowing && groupDMName ? groupDMName : ''}>
													<span ref={groupDMNameRef} className={styles.channelName}>
														{groupDMName}
													</span>
												</Tooltip>
											</div>
										) : (
											<FocusRing offset={-2}>
												<div
													className={styles.groupDMHeaderTrigger}
													role="button"
													tabIndex={0}
													onClick={handleOpenEditGroup}
													onKeyDown={(event) => {
														if (event.key === 'Enter' || event.key === ' ') {
															event.preventDefault();
															handleOpenEditGroup();
														}
													}}
												>
													<div className={styles.groupDMHeaderInner}>
														<GroupDMAvatar channel={channel} size={32} />
														<div className={styles.dmNameWrapper}>
															<Tooltip text={isGroupDMNameOverflowing && groupDMName ? groupDMName : ''}>
																<span
																	ref={groupDMNameRef}
																	className={clsx(styles.channelName, styles.groupDMChannelName)}
																>
																	{groupDMName}
																</span>
															</Tooltip>
														</div>
													</div>
													<PencilIcon className={styles.groupDMEditIcon} size={16} weight="bold" />
												</div>
											</FocusRing>
										)
									) : isPersonalNotes ? (
										<div className={styles.avatarWrapper}>
											{ChannelUtils.getIcon(channel, {className: styles.channelIcon})}
											<Tooltip text={isGuildChannelNameOverflowing && channelName ? channelName : ''}>
												<span ref={guildChannelNameRef} className={styles.channelName}>
													{channelName}
												</span>
											</Tooltip>
										</div>
									) : (
										// biome-ignore lint/a11y/noStaticElementInteractions: Context menu requires onContextMenu handler on this container
										<div className={styles.channelInfoContainer} onContextMenu={handleContextMenu}>
											{ChannelUtils.getIcon(channel, {className: styles.channelIcon})}
											<Tooltip text={isGuildChannelNameOverflowing && channelName ? channelName : ''}>
												<span ref={guildChannelNameRef} className={styles.channelName}>
													{channelName}
												</span>
											</Tooltip>

											{channel.topic && (
												<>
													<span className={styles.topicDivider}>•</span>
													<div className={styles.topicContainer}>
														<FocusRing offset={-2}>
															<div
																role="button"
																ref={topicButtonRef}
																className={clsx(
																	markupStyles.markup,
																	styles.topicButton,
																	isTopicOverflowing && styles.topicButtonOverflow,
																)}
																onClick={() =>
																	ModalActionCreators.push(modal(() => <ChannelTopicModal channelId={channel.id} />))
																}
																onKeyDown={(e) =>
																	e.key === 'Enter' &&
																	ModalActionCreators.push(modal(() => <ChannelTopicModal channelId={channel.id} />))
																}
																tabIndex={0}
															>
																<SafeMarkdown
																	content={channel.topic}
																	options={{
																		context: MarkdownContext.RESTRICTED_INLINE_REPLY,
																		channelId: channel.id,
																	}}
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

						<div className={styles.headerRightSection}>
							{isMobile && channel && !isPersonalNotes && AccessibilityStore.showFavorites && (
								<FocusRing offset={-2}>
									<button
										type="button"
										className={styles.iconButtonMobile}
										aria-label={isFavorited ? t`Remove from Favorites` : t`Add to Favorites`}
										onClick={handleToggleFavorite}
										onContextMenu={handleFavoriteContextMenu}
									>
										<StarIcon className={styles.buttonIconMobile} weight={isFavorited ? 'fill' : 'bold'} />
									</button>
								</FocusRing>
							)}

							{isMobile && (isDM || isGroupDM) && !isPersonalNotes && (
								<>
									<FocusRing offset={-2}>
										<button
											type="button"
											className={styles.iconButtonMobile}
											aria-label={t`Voice Call`}
											onClick={handleMobileVoiceCall}
										>
											<PhoneIcon className={styles.buttonIconMobile} />
										</button>
									</FocusRing>
									<FocusRing offset={-2}>
										<button
											type="button"
											className={styles.iconButtonMobile}
											aria-label={t`Video Call`}
											onClick={handleMobileVideoCall}
										>
											<VideoCameraIcon className={styles.buttonIconMobile} />
										</button>
									</FocusRing>
								</>
							)}

							{isMobile && isGuildChannel && (
								<FocusRing offset={-2}>
									<button
										type="button"
										className={styles.iconButtonMobile}
										aria-label={t`Search`}
										onClick={handleSearchClick}
									>
										<MagnifyingGlassIcon className={styles.buttonIconMobile} weight="bold" />
									</button>
								</FocusRing>
							)}

							{channel && !isMobile && !isPersonalNotes && AccessibilityStore.showFavorites && (
								<Tooltip text={isFavorited ? t`Remove from Favorites` : t`Add to Favorites`} position="bottom">
									<FocusRing offset={-2}>
										<button
											type="button"
											className={isFavorited ? styles.iconButtonSelected : styles.iconButtonDefault}
											aria-label={isFavorited ? t`Remove from Favorites` : t`Add to Favorites`}
											onClick={handleToggleFavorite}
											onContextMenu={handleFavoriteContextMenu}
										>
											<StarIcon className={styles.buttonIcon} weight={isFavorited ? 'fill' : 'bold'} />
										</button>
									</FocusRing>
								</Tooltip>
							)}

							{channel && isGuildChannel && !isMobile && !isVoiceChannel && !isPersonalNotes && (
								<ChannelNotificationSettingsButton channel={channel} />
							)}

							{showPins && channel && !isMobile && <ChannelPinsButton channel={channel} />}

							{(isDM || isGroupDM) && channel && !isMobile && !(isDM && isBotDMRecipient) && (
								<>
									<VoiceCallButton channel={channel} />
									<VideoCallButton channel={channel} />
								</>
							)}

							{shouldShowCreateGroupButton && (
								<ChannelHeaderIcon icon={UserPlusIcon} label={t`Create Group DM`} onClick={handleOpenCreateGroupDM} />
							)}

							{shouldShowAddFriendsButton && (
								<ChannelHeaderIcon
									icon={UserPlusIcon}
									label={t`Add Friends to Group`}
									onClick={handleOpenAddFriendsToGroup}
								/>
							)}

							{showMembersToggle && !isMobile && (
								<ChannelHeaderIcon
									icon={UsersIcon}
									isSelected={isMembersOpen}
									label={
										!canFitMemberList
											? t`Members list unavailable at this screen width`
											: isMembersOpen
												? t`Hide Members`
												: t`Show Members`
									}
									onClick={handleToggleMembers}
									disabled={!canFitMemberList}
									keybindAction="toggle_channel_member_list"
								/>
							)}

							{!isMobile && channel && !isVoiceChannel && (
								<FocusRing offset={-2} within>
									<div className={styles.messageSearchFocusWrapper}>
										<MessageSearchBar
											channel={channel}
											value={searchQuery}
											onChange={(query, segments) => {
												setSearchQuery(query);
												setSearchSegments(segments);
												latestSearchQueryRef.current = query;
												latestSearchSegmentsRef.current = segments;
											}}
											onSearch={() => {
												const q = latestSearchQueryRef.current;
												if (q.trim()) {
													onSearchSubmit?.(q, latestSearchSegmentsRef.current);
												}
											}}
											onClear={() => {
												setSearchQuery('');
												setSearchSegments([]);
												latestSearchQueryRef.current = '';
												latestSearchSegmentsRef.current = [];
												onSearchClose?.();
											}}
											isResultsOpen={Boolean(isSearchResultsOpen)}
											onCloseResults={() => onSearchClose?.()}
											inputRefExternal={searchInputRef}
										/>
									</div>
								</FocusRing>
							)}

							{!isMobile && <UpdaterIcon />}

							{!isMobile && <InboxButton />}
						</div>
					</NativeDragRegion>
				</div>

				{channel && (
					<ChannelDetailsBottomSheet
						isOpen={channelDetailsOpen}
						onClose={() => {
							setChannelDetailsOpen(false);
							setOpenSearchImmediately(false);
							setInitialTab('members');
						}}
						channel={channel}
						initialTab={initialTab}
						openSearchImmediately={openSearchImmediately}
					/>
				)}
			</>
		);
	},
);
