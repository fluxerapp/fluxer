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

import {Plural, Trans, useLingui} from '@lingui/react/macro';

import {
	CaretDownIcon,
	ChatTeardropIcon,
	CheckCircleIcon,
	ClockCounterClockwiseIcon,
	DotsThreeIcon,
	PencilIcon,
	ProhibitIcon,
	UserMinusIcon,
	UserPlusIcon,
	UsersThreeIcon,
} from '@phosphor-icons/react';

import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {useId} from 'react';
import type {PressEvent} from 'react-aria-components';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as NavigationActionCreators from '~/actions/NavigationActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as UserNoteActionCreators from '~/actions/UserNoteActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {DEFAULT_ACCENT_COLOR, Permissions, RelationshipTypes} from '~/Constants';
import {UserTag} from '~/components/channel/UserTag';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {CustomStatusModal} from '~/components/modals/CustomStatusModal';
import type {IARContext} from '~/components/modals/IARModal';
import {IARModal} from '~/components/modals/IARModal';
import * as Modal from '~/components/modals/Modal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {UserProfileBadges} from '~/components/popouts/UserProfileBadges';
import {UserProfileDataWarning} from '~/components/popouts/UserProfileDataWarning';
import {UserProfileBio, UserProfileMembershipInfo, UserProfileRoles} from '~/components/popouts/UserProfileShared';
import {Button} from '~/components/uikit/Button/Button';
import {
	BlockUserIcon,
	CopyFluxerTagIcon,
	CopyUserIdIcon,
	ReportUserIcon,
	VideoCallIcon,
	ViewGlobalProfileIcon,
	VoiceCallIcon,
} from '~/components/uikit/ContextMenu/ContextMenuIcons';
import {GroupDMContextMenu} from '~/components/uikit/ContextMenu/GroupDMContextMenu';
import {GuildContextMenu} from '~/components/uikit/ContextMenu/GuildContextMenu';
import {GuildMemberContextMenu} from '~/components/uikit/ContextMenu/GuildMemberContextMenu';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {MenuItemRadio} from '~/components/uikit/ContextMenu/MenuItemRadio';
import {UserContextMenu} from '~/components/uikit/ContextMenu/UserContextMenu';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tabs} from '~/components/uikit/Tabs/Tabs';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useAutoplayExpandedProfileAnimations} from '~/hooks/useAutoplayExpandedProfileAnimations';
import {TextareaAutosize} from '~/lib/TextareaAutosize';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {ProfileRecord} from '~/records/ProfileRecord';
import {type UserPartial, UserRecord} from '~/records/UserRecord';

import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import type {ContextMenuTargetElement} from '~/stores/ContextMenuStore';
import ContextMenuStore, {isContextMenuNodeTarget} from '~/stores/ContextMenuStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MemberPresenceSubscriptionStore from '~/stores/MemberPresenceSubscriptionStore';
import ModalStore from '~/stores/ModalStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserNoteStore from '~/stores/UserNoteStore';
import UserProfileStore from '~/stores/UserProfileStore';
import UserStore from '~/stores/UserStore';

import * as CallUtils from '~/utils/CallUtils';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as ColorUtils from '~/utils/ColorUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as ProfileDisplayUtils from '~/utils/ProfileDisplayUtils';
import {createMockProfile} from '~/utils/ProfileUtils';
import * as RelationshipActionUtils from '~/utils/RelationshipActionUtils';
import * as RouterUtils from '~/utils/RouterUtils';

import modalRootStyles from './Modal.module.css';
import userProfileModalStyles from './UserProfileModal.module.css';

export interface UserProfileModalProps {
	userId: string;
	guildId?: string;
	autoFocusNote?: boolean;
	disableEditProfile?: boolean;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
	previewUser?: UserRecord;
}

interface UserInfoProps {
	user: UserRecord;
	profile: ProfileRecord;
	guildId?: string;
	warningIndicator?: React.ReactNode;
	isCurrentUser?: boolean;
	onEditCustomStatus?: () => void;
}

interface UserNoteEditorProps {
	userId: string;
	initialNote: string | null;
	autoFocus?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
}

interface ProfileContentProps {
	profile: ProfileRecord;
	user: UserRecord;
	userNote: string | null;
	autoFocusNote?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
}

type UserProfileModalComponent = React.FC<UserProfileModalProps>;

interface ProfileModalContentProps {
	profile: ProfileRecord;
	user: UserRecord;
	userNote: string | null;
	autoFocusNote?: boolean;
	noteRef?: React.RefObject<HTMLTextAreaElement | null>;
	renderActionButtons: () => React.ReactNode;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
	warningIndicator?: React.ReactNode;
}

const UserInfo: React.FC<UserInfoProps> = observer(
	({user, profile, guildId, warningIndicator, isCurrentUser, onEditCustomStatus}) => {
		const displayName = NicknameUtils.getNickname(user, guildId);
		const effectiveProfile = profile?.getEffectiveProfile() ?? null;
		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();

		return (
			<div className={userProfileModalStyles.userInfo}>
				<div className={clsx(userProfileModalStyles.userInfoHeader, userProfileModalStyles.userInfoHeaderDesktop)}>
					<div className={userProfileModalStyles.userInfoContent}>
						<div className={userProfileModalStyles.nameRow}>
							<span className={userProfileModalStyles.userName}>{displayName}</span>
							{user.bot && <UserTag className={userProfileModalStyles.userTag} system={user.system} size="lg" />}
						</div>
						<div className={userProfileModalStyles.tagBadgeRow}>
							<div className={userProfileModalStyles.usernameRow}>{user.tag}</div>
							<div className={userProfileModalStyles.badgesWrapper}>
								<UserProfileBadges
									user={user}
									profile={profile}
									isModal={true}
									isMobile={false}
									warningIndicator={warningIndicator}
								/>
							</div>
						</div>
						{effectiveProfile?.pronouns && (
							<div className={userProfileModalStyles.pronouns}>{effectiveProfile.pronouns}</div>
						)}
						<div className={userProfileModalStyles.customStatusRow}>
							<CustomStatusDisplay
								userId={user.id}
								className={userProfileModalStyles.customStatusText}
								showTooltip
								allowJumboEmoji
								maxLines={0}
								isEditable={isCurrentUser}
								onEdit={onEditCustomStatus}
								showPlaceholder={isCurrentUser}
								alwaysAnimate={shouldAutoplayProfileAnimations}
							/>
						</div>
					</div>
				</div>
			</div>
		);
	},
);

const UserNoteEditor: React.FC<UserNoteEditorProps> = observer(({userId, initialNote, autoFocus, noteRef}) => {
	const {t} = useLingui();
	const [isEditing, setIsEditing] = React.useState(false);
	const [localNote, setLocalNote] = React.useState<string | null>(null);
	const internalNoteRef = React.useRef<HTMLTextAreaElement | null>(null);
	const textareaRef = noteRef || internalNoteRef;

	React.useEffect(() => {
		if (autoFocus && textareaRef.current) {
			setIsEditing(true);
		}
	}, [autoFocus, textareaRef]);

	const handleBlur = () => {
		if (localNote != null && localNote !== initialNote) {
			UserNoteActionCreators.update(userId, localNote);
		}
		setIsEditing(false);
	};

	const handleFocus = () => {
		setIsEditing(true);
		if (textareaRef.current) {
			const length = textareaRef.current.value.length;
			textareaRef.current.setSelectionRange(length, length);
		}
	};

	return (
		<div className={userProfileModalStyles.userNoteEditor}>
			<span className={userProfileModalStyles.noteLabel}>
				<Trans>Note</Trans>
			</span>
			<TextareaAutosize
				ref={textareaRef}
				aria-label={t`Note`}
				className={clsx(
					userProfileModalStyles.noteTextarea,
					userProfileModalStyles.noteTextareaBase,
					isEditing ? userProfileModalStyles.noteTextareaEditing : userProfileModalStyles.noteTextareaNotEditing,
				)}
				defaultValue={initialNote ?? undefined}
				maxLength={256}
				onBlur={handleBlur}
				onChange={(event) => setLocalNote(event.target.value)}
				onFocus={handleFocus}
				placeholder={isEditing ? undefined : t`Click to add a note`}
			/>
		</div>
	);
});

const ProfileContent: React.FC<ProfileContentProps> = observer(({profile, user, userNote, autoFocusNote, noteRef}) => {
	const guildMember = GuildMemberStore.getMember(profile?.guildId ?? '', user.id);
	const memberRoles = profile?.guildId && guildMember ? guildMember.getSortedRoles() : [];
	const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId: profile?.guild?.id});

	return (
		<div className={userProfileModalStyles.profileContent}>
			<div className={userProfileModalStyles.profileContentHeader}>
				<UserProfileBio profile={profile} />
				<UserProfileMembershipInfo profile={profile} user={user} />
				<UserProfileRoles
					profile={profile}
					user={user}
					memberRoles={[...memberRoles]}
					canManageRoles={canManageRoles}
				/>
				<UserNoteEditor userId={user.id} initialNote={userNote} autoFocus={autoFocusNote} noteRef={noteRef} />
			</div>
		</div>
	);
});

const ProfileModalContent: React.FC<ProfileModalContentProps> = observer(
	({profile, user, userNote, autoFocusNote, noteRef, renderActionButtons, previewOverrides, warningIndicator}) => {
		const {t} = useLingui();
		const effectiveProfile = profile?.getEffectiveProfile() ?? null;
		const rawAccentColor = effectiveProfile?.accent_color;
		const accentColorHex = typeof rawAccentColor === 'number' ? ColorUtils.int2hex(rawAccentColor) : rawAccentColor;
		const bannerColor = accentColorHex || DEFAULT_ACCENT_COLOR;

		const guildMember = GuildMemberStore.getMember(profile?.guildId ?? '', user.id);

		const profileContext = React.useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
			() => ({
				user,
				profile,
				guildId: profile?.guildId,
				guildMember,
				guildMemberProfile: profile?.guildMemberProfile,
			}),
			[user, profile, guildMember],
		);

		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();

		const {avatarUrl, hoverAvatarUrl} = React.useMemo(
			() => ProfileDisplayUtils.getProfileAvatarUrls(profileContext, previewOverrides),
			[profileContext, previewOverrides],
		);

		const bannerUrl = React.useMemo(
			() => ProfileDisplayUtils.getProfileBannerUrl(profileContext, previewOverrides, shouldAutoplayProfileAnimations),
			[profileContext, previewOverrides, shouldAutoplayProfileAnimations],
		);

		type MutualView = 'mutual_friends' | 'mutual_communities' | 'mutual_groups';

		const [activeTab, setActiveTab] = React.useState<'overview' | 'mutual'>('overview');
		const handleTabChange = React.useCallback((tab: 'overview' | 'mutual') => {
			setActiveTab(tab);
		}, []);

		const showMutualFriendsTab = !user.bot;
		const mutualFriendsCount = profile?.mutualFriends?.length ?? 0;
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;

		const openCustomStatus = React.useCallback(() => {
			ModalActionCreators.push(modal(() => <CustomStatusModal />));
		}, []);

		const profileMutualGuilds = profile?.mutualGuilds ?? [];
		type MutualGuildDisplay = {
			guild: GuildRecord;
			nick: string | null;
		};
		const mutualGuildDisplayItems = React.useMemo(() => {
			return profileMutualGuilds
				.map((mutualGuild) => {
					const guild = GuildStore.getGuild(mutualGuild.id);
					if (!guild) {
						return null;
					}
					return {guild, nick: mutualGuild.nick};
				})
				.filter((item): item is MutualGuildDisplay => item !== null);
		}, [profileMutualGuilds]);
		const mutualGroups = ChannelStore.dmChannels.filter(
			(channel) => channel.isGroupDM() && channel.recipientIds.includes(user.id),
		);

		const [mutualView, setMutualView] = React.useState<MutualView>(
			showMutualFriendsTab ? 'mutual_friends' : 'mutual_communities',
		);

		const mutualMenuButtonRef = React.useRef<HTMLButtonElement>(null);
		const [isMutualMenuOpen, setIsMutualMenuOpen] = React.useState(false);

		const getMutualViewLabel = React.useCallback(
			(view: MutualView) => {
				switch (view) {
					case 'mutual_friends': {
						const count = mutualFriendsCount;
						return t`Mutual Friends (${count})`;
					}
					case 'mutual_groups': {
						const count = mutualGroups.length;
						return t`Mutual Groups (${count})`;
					}
				default: {
					const count = profileMutualGuilds.length;
					return t`Mutual Communities (${count})`;
				}
			}
		},
		[t, mutualFriendsCount, mutualGroups.length, profileMutualGuilds.length],
	);

		const openMutualMenu = React.useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				const contextMenu = ContextMenuStore.contextMenu;
				const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;

				if (isOpen) {
					return;
				}

				setActiveTab('mutual');

				ContextMenuActionCreators.openFromEvent(event, () => (
					<MenuGroup>
						{showMutualFriendsTab && (
							<MenuItemRadio
								selected={mutualView === 'mutual_friends'}
								closeOnSelect
								onSelect={() => setMutualView('mutual_friends')}
							>
								{getMutualViewLabel('mutual_friends')}
							</MenuItemRadio>
						)}
						<MenuItemRadio
							selected={mutualView === 'mutual_communities'}
							closeOnSelect
							onSelect={() => setMutualView('mutual_communities')}
						>
							{getMutualViewLabel('mutual_communities')}
						</MenuItemRadio>
						<MenuItemRadio
							selected={mutualView === 'mutual_groups'}
							closeOnSelect
							onSelect={() => setMutualView('mutual_groups')}
						>
							{getMutualViewLabel('mutual_groups')}
						</MenuItemRadio>
					</MenuGroup>
				));
			},
			[getMutualViewLabel, mutualView, showMutualFriendsTab],
		);

		const mutualTabLabelText = React.useMemo(() => getMutualViewLabel(mutualView), [getMutualViewLabel, mutualView]);

		const tabs = React.useMemo(
			() =>
				[
					{key: 'overview', label: t`Overview`},
					{key: 'mutual', label: mutualTabLabelText},
				] as Array<{key: 'overview' | 'mutual'; label: React.ReactNode}>,
			[t, mutualTabLabelText],
		);

		React.useEffect(() => {
			setMutualView(showMutualFriendsTab ? 'mutual_friends' : 'mutual_communities');
		}, [showMutualFriendsTab, user.id]);

		React.useEffect(() => {
			if (!showMutualFriendsTab && mutualView === 'mutual_friends') {
				setMutualView('mutual_communities');
			}
		}, [mutualView, showMutualFriendsTab]);

		const handleMutualFriendClick = (friendId: string) => {
			const currentModal = ModalStore.getModal();
			if (currentModal) {
				ModalActionCreators.update(currentModal.key, () =>
					modal(() => <UserProfileModal userId={friendId} guildId={profile?.guildId ?? undefined} />),
				);
			}
		};

		const handleMutualFriendContextMenu = (event: React.MouseEvent, friend: UserRecord) => {
			event.preventDefault();
			event.stopPropagation();

			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<>
					{profile?.guildId ? (
						<GuildMemberContextMenu user={friend} guildId={profile.guildId} onClose={onClose} />
					) : (
						<UserContextMenu user={friend} onClose={onClose} />
					)}
				</>
			));
		};

		const handleGuildClick = (guild: GuildRecord) => {
			ModalActionCreators.pop();
			const selectedChannel = SelectedChannelStore.selectedChannelIds.get(guild.id);
			RouterUtils.transitionTo(Routes.guildChannel(guild.id, selectedChannel));
			NavigationActionCreators.selectGuild(guild.id);
		};

		const handleGuildContextMenu = (event: React.MouseEvent, guild: GuildRecord) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, (props) => (
				<GuildContextMenu guild={guild} onClose={props.onClose} />
			));
		};

		const handleGroupClick = (group: ChannelRecord) => {
			ModalActionCreators.pop();
			RouterUtils.transitionTo(Routes.dmChannel(group.id));
		};

		const handleGroupContextMenu = (event: React.MouseEvent, group: ChannelRecord) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<GroupDMContextMenu channel={group} onClose={onClose} />
			));
		};

		const [contextMenuTarget, setContextMenuTarget] = React.useState<ContextMenuTargetElement | null>(null);

		React.useEffect(() => {
			const disposer = autorun(() => {
				const contextMenu = ContextMenuStore.contextMenu;
				setContextMenuTarget(contextMenu?.target.target ?? null);
			});

			return () => {
				disposer();
			};
		}, []);

		const isContextMenuOpenFor = (target: EventTarget | null) => {
			if (!contextMenuTarget || !target) {
				return false;
			}
			if (target === contextMenuTarget) {
				return true;
			}
			if (target instanceof Node && isContextMenuNodeTarget(contextMenuTarget)) {
				return target.contains(contextMenuTarget);
			}
			return false;
		};

		React.useEffect(() => {
			const disposer = autorun(() => {
				const contextMenu = ContextMenuStore.contextMenu;
				const isOpen =
					!!contextMenu && !!mutualMenuButtonRef.current && contextMenu.target.target === mutualMenuButtonRef.current;
				setIsMutualMenuOpen(isOpen);
			});

			return () => {
				disposer();
			};
		}, []);

		const renderMutualFriendsList = React.useCallback(() => {
			const friends = profile?.mutualFriends ?? [];

			return (
				<div className={userProfileModalStyles.mutualFriendsList}>
					{friends.map((friend: UserPartial) => {
						const friendRecord = new UserRecord(friend);
						return (
							<MutualFriendItem
								key={friendRecord.id}
								user={friendRecord}
								profile={profile}
								onClick={() => handleMutualFriendClick(friendRecord.id)}
								onContextMenu={(e) => handleMutualFriendContextMenu(e, friendRecord)}
								isContextMenuOpen={isContextMenuOpenFor}
							/>
						);
					})}
					{friends.length === 0 && (
						<div className={userProfileModalStyles.emptyState}>
							<UsersThreeIcon className={userProfileModalStyles.emptyStateIcon} />
							<Trans>No mutual friends found.</Trans>
						</div>
					)}
				</div>
			);
		}, [handleMutualFriendClick, profile, isContextMenuOpenFor]);

		const renderMutualGroupsList = React.useCallback(() => {
			return (
				<div className={userProfileModalStyles.mutualFriendsList}>
					{mutualGroups.map((group) => (
						<MutualGroupItem
							key={group.id}
							group={group}
							onClick={() => handleGroupClick(group)}
							onContextMenu={(e) => handleGroupContextMenu(e, group)}
							isContextMenuOpen={isContextMenuOpenFor}
						/>
					))}
					{mutualGroups.length === 0 && (
						<div className={userProfileModalStyles.emptyState}>
							<UsersThreeIcon className={userProfileModalStyles.emptyStateIcon} />
							<Trans>No mutual groups found.</Trans>
						</div>
					)}
				</div>
			);
		}, [handleGroupClick, handleGroupContextMenu, isContextMenuOpenFor, mutualGroups]);

		const renderMutualGuildsList = React.useCallback(() => {
			return (
				<div className={userProfileModalStyles.mutualFriendsList}>
					{mutualGuildDisplayItems.map(({guild, nick}) => (
						<MutualGuildItem
							key={guild.id}
							guild={guild}
							nick={nick}
							onClick={() => handleGuildClick(guild)}
							onContextMenu={(e) => handleGuildContextMenu(e, guild)}
							isContextMenuOpen={isContextMenuOpenFor}
						/>
					))}
					{profileMutualGuilds.length === 0 && (
						<div className={userProfileModalStyles.emptyState}>
							<UsersThreeIcon className={userProfileModalStyles.emptyStateIcon} />
							<Trans>No mutual communities found.</Trans>
						</div>
					)}
				</div>
			);
		}, [
			handleGuildClick,
			handleGuildContextMenu,
			isContextMenuOpenFor,
			mutualGuildDisplayItems,
			profileMutualGuilds.length,
		]);

		const renderMutualTabContent = React.useCallback(() => {
			switch (mutualView) {
				case 'mutual_friends':
					return showMutualFriendsTab ? renderMutualFriendsList() : renderMutualGuildsList();
				case 'mutual_groups':
					return renderMutualGroupsList();
				default:
					return renderMutualGuildsList();
			}
		}, [mutualView, renderMutualFriendsList, renderMutualGroupsList, renderMutualGuildsList, showMutualFriendsTab]);

		const renderActiveTabContent = React.useCallback(() => {
			switch (activeTab) {
				case 'overview':
					return (
						<ProfileContent
							profile={profile}
							user={user}
							userNote={userNote}
							autoFocusNote={autoFocusNote}
							noteRef={noteRef}
						/>
					);
				default:
					return renderMutualTabContent();
			}
		}, [activeTab, autoFocusNote, noteRef, profile, renderMutualTabContent, user, userNote]);

		const reactId = useId();
		const safeId = reactId.replace(/[^a-zA-Z0-9_-]/g, '');
		const maskId = `uid_${safeId}`;

		return (
			<>
				<header>
					<div className={userProfileModalStyles.bannerContainer}>
						{/* biome-ignore lint/a11y/noSvgWithoutTitle: this is fine */}
						<svg className={userProfileModalStyles.bannerMask} viewBox="0 0 600 210" preserveAspectRatio="none">
							<mask id={maskId}>
								<rect fill="white" x="0" y="0" width="600" height="210" />
								<circle fill="black" cx="82" cy="210" r="66" />
							</mask>

							<foreignObject x="0" y="0" width="600" height="210" overflow="visible" mask={`url(#${maskId})`}>
								{bannerUrl ? (
									<div
										className={userProfileModalStyles.bannerImage}
										style={{
											backgroundImage: `url(${bannerUrl})`,
										}}
									/>
								) : (
									<div className={userProfileModalStyles.bannerColor} style={{backgroundColor: bannerColor}} />
								)}
							</foreignObject>
						</svg>
					</div>

					<div className={userProfileModalStyles.headerContainer}>
						<div className={userProfileModalStyles.avatarContainer}>
							<StatusAwareAvatar size={120} user={user} avatarUrl={avatarUrl} hoverAvatarUrl={hoverAvatarUrl} />
						</div>

						<div className={userProfileModalStyles.actionButtonsContainer}>{renderActionButtons()}</div>
					</div>
				</header>

				<div className={userProfileModalStyles.contentContainer}>
					<UserInfo
						user={user}
						profile={profile}
						guildId={profile.guildId ?? undefined}
						warningIndicator={warningIndicator}
						isCurrentUser={isCurrentUser}
						onEditCustomStatus={openCustomStatus}
					/>

					{!isCurrentUser ? (
						<div className={userProfileModalStyles.tabsWrapper}>
							<Tabs
								activeTab={activeTab}
								onTabChange={handleTabChange}
								tabs={tabs}
								renderTabSibling={(tab) =>
									tab === 'mutual' ? (
										<button
											ref={mutualMenuButtonRef}
											type="button"
											className={clsx(
												userProfileModalStyles.mutualMenuButton,
												isMutualMenuOpen && userProfileModalStyles.mutualMenuButtonActive,
											)}
											onClick={(event) => openMutualMenu(event)}
											aria-label={t`Select mutual view`}
										>
											<CaretDownIcon
												weight="bold"
												className={clsx(
													userProfileModalStyles.mutualMenuIcon,
													isMutualMenuOpen && userProfileModalStyles.mutualMenuIconOpen,
												)}
											/>
										</button>
									) : null
								}
							/>
						</div>
					) : (
						<div className={userProfileModalStyles.separator} />
					)}

					<div className={userProfileModalStyles.profileContentWrapper}>
						<Scroller className={userProfileModalStyles.scrollerFullHeight} key="user-profile-modal-content-scroller">
							{renderActiveTabContent()}
						</Scroller>
					</div>
				</div>
			</>
		);
	},
);

const MutualFriendItem = ({
	user,
	profile,
	onClick,
	onContextMenu,
	isContextMenuOpen,
}: {
	user: UserRecord;
	profile: ProfileRecord | null;
	onClick: () => void;
	onContextMenu: (e: React.MouseEvent) => void;
	isContextMenuOpen: (target: EventTarget | null) => boolean;
}) => {
	const itemRef = React.useRef<HTMLDivElement>(null);
	const isActive = isContextMenuOpen(itemRef.current);

	return (
		<div
			ref={itemRef}
			className={clsx(userProfileModalStyles.mutualFriendItem, isActive && userProfileModalStyles.active)}
			onClick={onClick}
			onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && onClick()}
			onContextMenu={onContextMenu}
			role="button"
			tabIndex={0}
		>
			<StatusAwareAvatar size={40} user={user} />
			<div className={userProfileModalStyles.mutualFriendInfo}>
				<span className={userProfileModalStyles.mutualFriendName}>
					{NicknameUtils.getNickname(user, profile?.guildId ?? undefined)}
				</span>
				<span className={userProfileModalStyles.mutualFriendUsername}>{user.tag}</span>
			</div>
		</div>
	);
};

const MutualGuildItem = ({
	guild,
	nick,
	onClick,
	onContextMenu,
	isContextMenuOpen,
}: {
	guild: GuildRecord;
	nick: string | null;
	onClick: () => void;
	onContextMenu: (e: React.MouseEvent) => void;
	isContextMenuOpen: (target: EventTarget | null) => boolean;
}) => {
	const itemRef = React.useRef<HTMLDivElement>(null);
	const isActive = isContextMenuOpen(itemRef.current);

	return (
		<div
			ref={itemRef}
			className={clsx(userProfileModalStyles.mutualFriendItem, isActive && userProfileModalStyles.active)}
			onClick={onClick}
			onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && onClick()}
			onContextMenu={onContextMenu}
			role="button"
			tabIndex={0}
		>
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				className={userProfileModalStyles.mutualGuildIcon}
				sizePx={40}
			/>
			<div className={userProfileModalStyles.mutualFriendInfo}>
				<span className={userProfileModalStyles.mutualFriendName}>{guild.name}</span>
				{nick && <span className={userProfileModalStyles.mutualFriendUsername}>{nick}</span>}
			</div>
		</div>
	);
};

const MutualGroupItem = ({
	group,
	onClick,
	onContextMenu,
	isContextMenuOpen,
}: {
	group: ChannelRecord;
	onClick: () => void;
	onContextMenu: (e: React.MouseEvent) => void;
	isContextMenuOpen: (target: EventTarget | null) => boolean;
}) => {
	const itemRef = React.useRef<HTMLDivElement>(null);
	const isActive = isContextMenuOpen(itemRef.current);

	return (
		<div
			ref={itemRef}
			className={clsx(userProfileModalStyles.mutualFriendItem, isActive && userProfileModalStyles.active)}
			onClick={onClick}
			onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && onClick()}
			onContextMenu={onContextMenu}
			role="button"
			tabIndex={0}
		>
			<GroupDMAvatar channel={group} size={40} />
			<div className={userProfileModalStyles.mutualFriendInfo}>
				<span className={userProfileModalStyles.mutualFriendName}>{ChannelUtils.getDMDisplayName(group)}</span>
				<span className={userProfileModalStyles.mutualFriendUsername}>
					<Plural value={group.recipientIds.length + 1} one="# Member" other="# Members" />
				</span>
			</div>
		</div>
	);
};

export const UserProfileModal: UserProfileModalComponent = observer(
	({userId, guildId, autoFocusNote, disableEditProfile, previewOverrides, previewUser}) => {
		const {t, i18n} = useLingui();

		const storeUser = UserStore.getUser(userId);
		const user = previewUser ?? storeUser;

		const fallbackUser = React.useMemo(
			() =>
				new UserRecord({
					id: userId,
					username: userId,
					discriminator: '0000',
					avatar: null,
					flags: 0,
				}),
			[userId],
		);

		const displayUser = user ?? fallbackUser;

		const fallbackProfile = React.useMemo(() => createMockProfile(fallbackUser), [fallbackUser]);
		const mockProfile = React.useMemo(() => (user ? createMockProfile(user) : null), [user]);
		const initialProfile = React.useMemo(() => UserProfileStore.getProfile(userId, guildId), [userId, guildId]);
		const [profile, setProfile] = React.useState<ProfileRecord | null>(initialProfile);
		const [profileLoadError, setProfileLoadError] = React.useState(false);
		const [showGlobalProfile, setShowGlobalProfile] = React.useState(false);
		const [isProfileLoading, setIsProfileLoading] = React.useState(() => !previewUser && !initialProfile);
		const userNote = UserNoteStore.getUserNote(userId);
		const isCurrentUser = user?.id === AuthenticationStore.currentUserId;
		const relationship = RelationshipStore.getRelationship(userId);
		const relationshipType = relationship?.type;
		const isUserBot = user?.bot ?? false;
		const noteRef = React.useRef<HTMLTextAreaElement | null>(null);
		const moreOptionsButtonRef = React.useRef<HTMLButtonElement>(null);
		const [isMoreMenuOpen, setIsMoreMenuOpen] = React.useState(false);

		React.useEffect(() => {
			setProfile(initialProfile);
			setIsProfileLoading(!previewUser && !initialProfile);
		}, [initialProfile, previewUser]);

		React.useEffect(() => {
			if (previewUser || profile) {
				setIsProfileLoading(false);
				setProfileLoadError(false);
				return;
			}

			let cancelled = false;
			setIsProfileLoading(true);
			setProfileLoadError(false);

			UserProfileActionCreators.fetch(userId, guildId)
				.then(() => {
					if (cancelled) return;
					const fetchedProfile = UserProfileStore.getProfile(userId, guildId);
					if (fetchedProfile) {
						setProfile(fetchedProfile);
					}
					setProfileLoadError(false);
				})
				.catch((error) => {
					if (cancelled) return;
					console.error('Failed to fetch user profile:', error);
					setProfileLoadError(true);
				})
				.finally(() => {
					if (cancelled) return;
					setIsProfileLoading(false);
				});

			return () => {
				cancelled = true;
			};
		}, [userId, guildId, previewUser, profile]);

		React.useEffect(() => {
			const handleContextMenuChange = () => {
				const contextMenu = ContextMenuStore.contextMenu;
				const isOpen =
					!!contextMenu && !!moreOptionsButtonRef.current && contextMenu.target.target === moreOptionsButtonRef.current;

				setIsMoreMenuOpen(isOpen);
			};

			const disposer = autorun(handleContextMenuChange);
			return () => disposer();
		}, []);

		React.useEffect(() => {
			if (!guildId || !userId || previewUser) {
				return;
			}

			const hasMember = GuildMemberStore.getMember(guildId, userId);
			if (!hasMember) {
				MemberPresenceSubscriptionStore.touchMember(guildId, userId);
				GuildMemberStore.fetchMembers(guildId, {userIds: [userId]}).catch((error) => {
					console.error('[UserProfileModal] Failed to fetch guild member:', error);
				});
			} else {
				MemberPresenceSubscriptionStore.touchMember(guildId, userId);
			}

			return () => {
				MemberPresenceSubscriptionStore.unsubscribe(guildId, userId);
			};
		}, [guildId, userId, previewUser]);

		const hasGuildProfile = !!(profile?.guildId && profile?.guildMemberProfile);
		const shouldShowProfileDataWarning = profileLoadError || DeveloperOptionsStore.forceProfileDataWarning;

		const displayProfile = React.useMemo((): ProfileRecord | null => {
			if (!profile) return null;
			if (showGlobalProfile && hasGuildProfile) {
				return profile.withUpdates({guild_member_profile: null}).withGuildId(null);
			}
			return profile;
		}, [profile, showGlobalProfile, hasGuildProfile]);

		const screenReaderLabel = React.useMemo(() => {
			if (!displayUser) return t`User Profile`;
			const tag = displayUser.tag;
			return t`User Profile: ${tag}`;
		}, [displayUser, t]);

		const shouldShowSpinner = isProfileLoading || !user;
		const effectiveProfile: ProfileRecord | null = displayProfile ?? profile ?? mockProfile;
		const resolvedProfile: ProfileRecord = effectiveProfile ?? fallbackProfile;

		const handleEditProfile = () => {
			ModalActionCreators.pop();
			ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="my_profile" />));
		};

		const handleMessage = async () => {
			try {
				ModalActionCreators.pop();
				await PrivateChannelActionCreators.openDMChannel(userId);
			} catch (error) {
				console.error('Failed to open DM channel:', error);
			}
		};

		const handleSendFriendRequest = () => {
			RelationshipActionUtils.sendFriendRequest(i18n, userId);
		};

		const handleAcceptFriendRequest = () => {
			RelationshipActionUtils.acceptFriendRequest(i18n, userId);
		};

		const handleRemoveFriend = () => {
			RelationshipActionUtils.showRemoveFriendConfirmation(i18n, displayUser);
		};

		const handleBlockUser = () => {
			RelationshipActionUtils.showBlockUserConfirmation(i18n, displayUser);
		};

		const handleUnblockUser = () => {
			RelationshipActionUtils.showUnblockUserConfirmation(i18n, displayUser);
		};

		const handleCancelFriendRequest = () => {
			RelationshipActionUtils.cancelFriendRequest(i18n, userId);
		};

		const handleStartVoiceCall = async (event?: PressEvent) => {
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(userId);
				await CallUtils.checkAndStartCall(channelId, event?.shiftKey ?? false);
			} catch (error) {
				console.error('Failed to start voice call:', error);
			}
		};

		const handleStartVideoCall = async (event?: PressEvent) => {
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(userId);
				await CallUtils.checkAndStartCall(channelId, event?.shiftKey ?? false);
			} catch (error) {
				console.error('Failed to start video call:', error);
			}
		};

		const handleReportUser = () => {
			const context: IARContext = {
				type: 'user',
				user: displayUser,
				guildId,
			};
			ModalActionCreators.push(modal(() => <IARModal context={context} />));
		};

		const handleCopyFluxerTag = () => {
			TextCopyActionCreators.copy(i18n, `${displayUser.username}#${displayUser.discriminator}`, true);
		};

		const handleCopyUserId = () => {
			TextCopyActionCreators.copy(i18n, displayUser.id, true);
		};

		const handleMoreOptionsPointerDown = (event: React.PointerEvent) => {
			const contextMenu = ContextMenuStore.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === moreOptionsButtonRef.current;

			if (isOpen) {
				event.stopPropagation();
				event.preventDefault();
				ContextMenuActionCreators.close();
			}
		};

		const renderBlockMenuItem = (onClose: () => void) => {
			switch (relationshipType) {
				case RelationshipTypes.BLOCKED:
					return (
						<MenuItem
							icon={<BlockUserIcon />}
							onClick={() => {
								handleUnblockUser();
								onClose();
							}}
						>
							{t`Unblock`}
						</MenuItem>
					);
				default:
					return (
						<MenuItem
							icon={<BlockUserIcon />}
							onClick={() => {
								handleBlockUser();
								onClose();
							}}
							danger
						>
							{t`Block`}
						</MenuItem>
					);
			}
		};

		const openMoreOptionsMenu = (event: React.MouseEvent<HTMLButtonElement>) => {
			const contextMenu = ContextMenuStore.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;

			if (isOpen) {
				return;
			}

			ContextMenuActionCreators.openFromEvent(event, (props) => (
				<>
					{hasGuildProfile && (
						<MenuGroup>
							<MenuItem
								icon={<ViewGlobalProfileIcon />}
								onClick={() => {
									setShowGlobalProfile(!showGlobalProfile);
									props.onClose();
								}}
							>
								{showGlobalProfile ? t`View Community Profile` : t`View Global Profile`}
							</MenuItem>
						</MenuGroup>
					)}
					{!isCurrentUser && !isUserBot && relationshipType === RelationshipTypes.FRIEND && (
						<MenuGroup>
							<MenuItem
								icon={<VoiceCallIcon />}
								onClick={(pressEvent: PressEvent) => {
									handleStartVoiceCall(pressEvent);
									props.onClose();
								}}
							>
								{t`Start Voice Call`}
							</MenuItem>
							<MenuItem
								icon={<VideoCallIcon />}
								onClick={(pressEvent: PressEvent) => {
									handleStartVideoCall(pressEvent);
									props.onClose();
								}}
							>
								{t`Start Video Call`}
							</MenuItem>
						</MenuGroup>
					)}
					<MenuGroup>
						<MenuItem
							icon={<CopyFluxerTagIcon />}
							onClick={() => {
								handleCopyFluxerTag();
								props.onClose();
							}}
						>
							{t`Copy FluxerTag`}
						</MenuItem>
						<MenuItem
							icon={<CopyUserIdIcon />}
							onClick={() => {
								handleCopyUserId();
								props.onClose();
							}}
						>
							{t`Copy User ID`}
						</MenuItem>
					</MenuGroup>
					{!isCurrentUser && relationshipType === RelationshipTypes.FRIEND && (
						<MenuGroup>
							<MenuItem
								icon={<UserMinusIcon className={userProfileModalStyles.menuIcon} weight="fill" />}
								onClick={() => {
									handleRemoveFriend();
									props.onClose();
								}}
								danger
							>
								{t`Remove Friend`}
							</MenuItem>
						</MenuGroup>
					)}
					{!isCurrentUser && (
						<MenuGroup>
							<MenuItem
								icon={<ReportUserIcon />}
								onClick={() => {
									handleReportUser();
									props.onClose();
								}}
								danger
							>
								{t`Report User`}
							</MenuItem>
							{renderBlockMenuItem(props.onClose)}
						</MenuGroup>
					)}
				</>
			));
		};

		const renderActionButtons = () => {
			const currentUserUnclaimed = !(UserStore.currentUser?.isClaimed() ?? true);
			if (isCurrentUser && disableEditProfile) {
				return (
					<div className={userProfileModalStyles.actionButtons}>
						<Tooltip text={t`You can't befriend yourself`} maxWidth="xl">
							<div>
								<Button
									variant="secondary"
									small={true}
									leftIcon={<UserPlusIcon className={userProfileModalStyles.buttonIcon} />}
									disabled={true}
								>
									<Trans>Add Friend</Trans>
								</Button>
							</div>
						</Tooltip>
						<Tooltip text={t`You can't message yourself`} maxWidth="xl">
							<div>
								<Button
									small={true}
									leftIcon={<ChatTeardropIcon className={userProfileModalStyles.buttonIcon} />}
									disabled={true}
								>
									<Trans>Message</Trans>
								</Button>
							</div>
						</Tooltip>
					</div>
				);
			}

			if (isCurrentUser && !disableEditProfile) {
				return (
					<div className={userProfileModalStyles.actionButtons}>
						<Button
							small={true}
							leftIcon={<PencilIcon className={userProfileModalStyles.buttonIcon} />}
							onClick={handleEditProfile}
						>
							<Trans>Edit Profile</Trans>
						</Button>
						<Button
							ref={moreOptionsButtonRef}
							small={true}
							square={true}
							variant="secondary"
							icon={<DotsThreeIcon className={userProfileModalStyles.buttonIcon} weight="bold" />}
							onPointerDownCapture={handleMoreOptionsPointerDown}
							onClick={openMoreOptionsMenu}
							className={isMoreMenuOpen ? userProfileModalStyles.moreMenuButtonActive : undefined}
						/>
					</div>
				);
			}

			const renderPrimaryActionButton = () => {
				if (isUserBot) {
					return null;
				}

				if (relationshipType === RelationshipTypes.FRIEND) {
					return (
						<Tooltip text={t`Remove Friend`} maxWidth="xl">
							<div>
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={<UserMinusIcon className={userProfileModalStyles.buttonIcon} />}
									onClick={handleRemoveFriend}
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === RelationshipTypes.BLOCKED) {
					return (
						<Tooltip text={t`Unblock User`} maxWidth="xl">
							<div>
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={<ProhibitIcon className={userProfileModalStyles.buttonIcon} />}
									onClick={handleUnblockUser}
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
					return (
						<Tooltip text={t`Accept Friend Request`} maxWidth="xl">
							<div>
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={<CheckCircleIcon className={userProfileModalStyles.buttonIcon} />}
									onClick={handleAcceptFriendRequest}
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
					return (
						<Tooltip text={t`Cancel Friend Request`} maxWidth="xl">
							<div>
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={<ClockCounterClockwiseIcon className={userProfileModalStyles.buttonIcon} />}
									onClick={handleCancelFriendRequest}
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === undefined && !isUserBot) {
					const tooltipText = currentUserUnclaimed
						? t`Claim your account to send friend requests.`
						: t`Send Friend Request`;
					return (
						<Tooltip text={tooltipText} maxWidth="xl">
							<div>
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={<UserPlusIcon className={userProfileModalStyles.buttonIcon} />}
									onClick={handleSendFriendRequest}
									disabled={currentUserUnclaimed}
								/>
							</div>
						</Tooltip>
					);
				}
				return null;
			};

			return (
				<div className={userProfileModalStyles.actionButtons}>
					<Button
						small={true}
						leftIcon={<ChatTeardropIcon className={userProfileModalStyles.buttonIcon} />}
						onClick={handleMessage}
					>
						<Trans>Message</Trans>
					</Button>
					{renderPrimaryActionButton()}
					<Button
						ref={moreOptionsButtonRef}
						small={true}
						square={true}
						variant="secondary"
						icon={<DotsThreeIcon className={userProfileModalStyles.buttonIcon} weight="bold" />}
						onPointerDownCapture={handleMoreOptionsPointerDown}
						onClick={openMoreOptionsMenu}
						className={isMoreMenuOpen ? userProfileModalStyles.moreMenuButtonActive : undefined}
					/>
				</div>
			);
		};

		const borderProfile = displayProfile?.getEffectiveProfile() ?? null;
		const rawAccentColor = borderProfile?.accent_color;
		const accentColorHex = typeof rawAccentColor === 'number' ? ColorUtils.int2hex(rawAccentColor) : rawAccentColor;
		const borderColor = accentColorHex || DEFAULT_ACCENT_COLOR;

		return (
			<Modal.Root
				size="medium"
				initialFocusRef={autoFocusNote ? noteRef : undefined}
				className={clsx(modalRootStyles.root, modalRootStyles.medium, userProfileModalStyles.modalRoot)}
			>
				<Modal.ScreenReaderLabel text={screenReaderLabel} />
				<div className={userProfileModalStyles.modalContainer} style={{borderColor}}>
					{shouldShowSpinner ? (
						<div className={userProfileModalStyles.loadingScreen}>
							<Spinner size="large" />
						</div>
					) : (
						<ProfileModalContent
							key={displayUser.id}
							profile={resolvedProfile}
							user={displayUser}
							userNote={userNote}
							autoFocusNote={autoFocusNote}
							noteRef={noteRef}
							renderActionButtons={renderActionButtons}
							warningIndicator={shouldShowProfileDataWarning ? <UserProfileDataWarning /> : undefined}
							previewOverrides={previewOverrides}
						/>
					)}
				</div>
			</Modal.Root>
		);
	},
);
