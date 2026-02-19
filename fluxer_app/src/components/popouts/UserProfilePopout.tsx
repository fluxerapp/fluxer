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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as PopoutActionCreators from '@app/actions/PopoutActionCreators';
import * as PrivateChannelActionCreators from '@app/actions/PrivateChannelActionCreators';
import * as UserProfileActionCreators from '@app/actions/UserProfileActionCreators';
import {CustomStatusDisplay} from '@app/components/common/custom_status_display/CustomStatusDisplay';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {UserProfileModal} from '@app/components/modals/UserProfileModal';
import {UserSettingsModal} from '@app/components/modals/UserSettingsModal';
import {UserProfileBadges} from '@app/components/popouts/UserProfileBadges';
import {UserProfileDataWarning} from '@app/components/popouts/UserProfileDataWarning';
import styles from '@app/components/popouts/UserProfilePopout.module.css';
import {
	UserProfileConnections,
	UserProfileMembershipInfo,
	UserProfilePreviewBio,
	UserProfileRoles,
} from '@app/components/popouts/UserProfileShared';
import {ProfileCardActions} from '@app/components/profile/profile_card/ProfileCardActions';
import {ProfileCardBanner} from '@app/components/profile/profile_card/ProfileCardBanner';
import {ProfileCardContent} from '@app/components/profile/profile_card/ProfileCardContent';
import {ProfileCardFooter} from '@app/components/profile/profile_card/ProfileCardFooter';
import {ProfileCardLayout} from '@app/components/profile/profile_card/ProfileCardLayout';
import {ProfileCardUserInfo} from '@app/components/profile/profile_card/ProfileCardUserInfo';
import {useProfileCardDisplayState} from '@app/components/profile/useProfileCardDisplayState';
import {VoiceActivitySection} from '@app/components/profile/VoiceActivitySection';
import {Button} from '@app/components/uikit/button/Button';
import FocusRingScope from '@app/components/uikit/focus_ring/FocusRingScope';
import {Spinner} from '@app/components/uikit/Spinner';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useAutoplayExpandedProfileAnimations} from '@app/hooks/useAutoplayExpandedProfileAnimations';
import {useHover} from '@app/hooks/useHover';
import {Logger} from '@app/lib/Logger';
import type {ProfileRecord} from '@app/records/ProfileRecord';
import type {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import DeveloperOptionsStore from '@app/stores/DeveloperOptionsStore';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import MemberPresenceSubscriptionStore from '@app/stores/MemberPresenceSubscriptionStore';
import PermissionStore from '@app/stores/PermissionStore';
import RelationshipStore from '@app/stores/RelationshipStore';
import UserProfileStore from '@app/stores/UserProfileStore';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import {createMockProfile} from '@app/utils/ProfileUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MEDIA_PROXY_PROFILE_BANNER_SIZE_POPOUT} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon, PencilIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('UserProfilePopout');

interface UserProfilePopoutProps {
	popoutKey: string | number;
	user: UserRecord;
	isWebhook: boolean;
	guildId?: string;
	isPreview?: boolean;
}

export const UserProfilePopout: React.FC<UserProfilePopoutProps> = observer(
	({popoutKey, user, isWebhook, guildId, isPreview}) => {
		const {t} = useLingui();
		const [hoverRef, isHovering] = useHover();
		const [profile, setProfile] = useState<ProfileRecord | null>(() => {
			const cachedProfile = UserProfileStore.getProfile(user.id, guildId);
			return cachedProfile ?? createMockProfile(user);
		});
		const [profileLoadError, setProfileLoadError] = useState(false);
		const showProfileDataWarning = !isWebhook && (profileLoadError || DeveloperOptionsStore.forceProfileDataWarning);

		const guildMember = GuildMemberStore.getMember(profile?.guildId ?? '', user.id);
		const memberRoles = profile?.guildId && guildMember ? guildMember.getSortedRoles() : [];
		const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId});
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;
		const relationshipType = RelationshipStore.getRelationship(user.id)?.type;
		const isBlocked = relationshipType === RelationshipTypes.BLOCKED;

		const openFullProfile = useCallback(
			(autoFocusNote?: boolean) => {
				if (isWebhook) {
					return;
				}

				ModalActionCreators.push(
					modal(() => <UserProfileModal userId={user.id} guildId={guildId} autoFocusNote={autoFocusNote} />),
				);
				PopoutActionCreators.close(popoutKey);
			},
			[isWebhook, user.id, guildId, popoutKey],
		);

		useEffect(() => {
			let cancelled = false;
			const cachedProfile = UserProfileStore.getProfile(user.id, guildId);
			setProfile(cachedProfile ?? createMockProfile(user));
			setProfileLoadError(false);

			if (isWebhook) {
				return () => {
					cancelled = true;
				};
			}

			const fetchProfile = async () => {
				const isGuildMember = guildId ? GuildMemberStore.getMember(guildId, user.id) : false;

				if (DeveloperOptionsStore.slowProfileLoad) {
					await new Promise((resolve) => setTimeout(resolve, 3000));
				}

				if (cancelled) {
					return;
				}

				try {
					const fetchedProfile = await UserProfileActionCreators.fetch(user.id, isGuildMember ? guildId : undefined);
					if (cancelled) {
						return;
					}
					setProfile(fetchedProfile);
					setProfileLoadError(false);
				} catch (error) {
					if (cancelled) {
						return;
					}
					logger.error('Failed to fetch profile for user popout', error);
					const nextCachedProfile = UserProfileStore.getProfile(user.id, guildId);
					setProfile(nextCachedProfile ?? createMockProfile(user));
					setProfileLoadError(true);
				}
			};

			fetchProfile();

			return () => {
				cancelled = true;
			};
		}, [guildId, isWebhook, user]);

		useEffect(() => {
			if (profileLoadError && profile) {
				const profileData = profile?.getEffectiveProfile() ?? null;
				if (profileData && Object.keys(profileData).length > 0) {
					setProfileLoadError(false);
				}
			}
		}, [profileLoadError, profile]);

		useEffect(() => {
			if (!guildId || !user.id || isWebhook) {
				return;
			}

			const hasMember = GuildMemberStore.getMember(guildId, user.id);
			if (!hasMember) {
				MemberPresenceSubscriptionStore.touchMember(guildId, user.id);
				GuildMemberStore.fetchMembers(guildId, {userIds: [user.id]}).catch((error) => {
					logger.error('Failed to fetch guild member', error);
				});
			} else {
				MemberPresenceSubscriptionStore.touchMember(guildId, user.id);
			}

			return () => {
				MemberPresenceSubscriptionStore.unsubscribe(guildId, user.id);
			};
		}, [guildId, user.id, isWebhook]);

		const handleEditProfile = () => {
			ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="my_profile" />));
			PopoutActionCreators.close(popoutKey);
		};

		const handleClosePopout = useCallback(() => {
			PopoutActionCreators.close(popoutKey);
		}, [popoutKey]);

		const handleMessage = async () => {
			try {
				PopoutActionCreators.close(popoutKey);
				await PrivateChannelActionCreators.openDMChannel(user.id);
			} catch (error) {
				logger.error('Failed to open DM channel', error);
			}
		};

		const handleOpenBlockedDm = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Open DM`}
						description={t`You blocked ${user.username}. You won't be able to send messages unless you unblock them.`}
						primaryText={t`Open DM`}
						primaryVariant="primary"
						onPrimary={handleMessage}
					/>
				)),
			);
		};

		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();

		const {avatarUrl, hoverAvatarUrl, bannerUrl, accentColor, profileData} = useProfileCardDisplayState({
			user,
			profile,
			guildId: profile?.guildId,
			guildMember,
			guildMemberProfile: profile?.guildMemberProfile,
			shouldAutoplayProfileAnimations,
			bannerSize: MEDIA_PROXY_PROFILE_BANNER_SIZE_POPOUT,
		});

		const popoutContainerRef = useRef<HTMLDivElement | null>(null);
		const displayName = NicknameUtils.getNickname(user, guildId);

		if (!profile && !isWebhook) {
			return (
				<div className={styles.loadingContainer}>
					<Spinner />
				</div>
			);
		}

		const borderColor = accentColor;
		const bannerColor = accentColor;

		return (
			<FocusRingScope containerRef={popoutContainerRef}>
				<div ref={popoutContainerRef}>
					<ProfileCardLayout borderColor={borderColor} hoverRef={hoverRef}>
						<ProfileCardBanner
							bannerUrl={bannerUrl as string | null}
							bannerColor={bannerColor}
							user={user}
							avatarUrl={avatarUrl}
							hoverAvatarUrl={hoverAvatarUrl}
							disablePresence={isWebhook}
							isClickable={!isWebhook}
							onAvatarClick={!isWebhook ? () => openFullProfile() : undefined}
						/>

						{!isWebhook && <UserProfileBadges user={user} profile={profile} />}

						<ProfileCardContent isWebhook={isWebhook}>
							{showProfileDataWarning && (
								<div className={styles.profileDataWarning}>
									<UserProfileDataWarning />
								</div>
							)}
							<ProfileCardUserInfo
								displayName={displayName}
								user={user}
								pronouns={profileData?.pronouns}
								showUsername={!isWebhook}
								isClickable={!isWebhook}
								isWebhook={isWebhook}
								onDisplayNameClick={!isWebhook ? () => openFullProfile() : undefined}
								onUsernameClick={!isWebhook ? () => openFullProfile() : undefined}
								actions={
									!isWebhook && (
										<ProfileCardActions
											userId={user.id}
											isHovering={isHovering}
											onNoteClick={() => openFullProfile(true)}
										/>
									)
								}
							/>
							{!isWebhook && (
								<div className={styles.profileCustomStatus}>
									<CustomStatusDisplay
										userId={user.id}
										className={styles.profileCustomStatusText}
										allowJumboEmoji
										maxLines={0}
										alwaysAnimate={shouldAutoplayProfileAnimations}
									/>
								</div>
							)}
							{!isWebhook && <VoiceActivitySection userId={user.id} onNavigate={handleClosePopout} />}
							{profile && (
								<UserProfilePreviewBio
									profile={profile}
									profileData={profileData ?? null}
									onShowMore={() => openFullProfile()}
								/>
							)}
							{profile && <UserProfileMembershipInfo profile={profile} user={user} />}
							{!isWebhook && profile && (
								<UserProfileRoles
									profile={profile}
									user={user}
									memberRoles={[...memberRoles]}
									canManageRoles={canManageRoles}
								/>
							)}
							{profile && <UserProfileConnections profile={profile} variant="compact" />}
						</ProfileCardContent>

						{!isWebhook && (
							<ProfileCardFooter>
								{isCurrentUser ? (
									isPreview ? (
										<Tooltip text={t`You can't message yourself`} maxWidth="xl">
											<div>
												<Button
													small={true}
													leftIcon={<ChatTeardropIcon className={styles.iconSmall} />}
													disabled={true}
												>
													<Trans>Message</Trans>
												</Button>
											</div>
										</Tooltip>
									) : (
										<Button
											small={true}
											leftIcon={<PencilIcon className={styles.iconSmall} />}
											onClick={handleEditProfile}
										>
											<Trans>Edit Profile</Trans>
										</Button>
									)
								) : (
									<Button
										small={true}
										leftIcon={<ChatTeardropIcon className={styles.iconSmall} />}
										onClick={isBlocked ? handleOpenBlockedDm : handleMessage}
									>
										{isBlocked ? <Trans>Open DM</Trans> : <Trans>Message</Trans>}
									</Button>
								)}
							</ProfileCardFooter>
						)}
					</ProfileCardLayout>
				</div>
			</FocusRingScope>
		);
	},
);
