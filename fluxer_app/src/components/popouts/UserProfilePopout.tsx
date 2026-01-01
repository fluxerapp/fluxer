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

import {Trans, useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon, PencilIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {DEFAULT_ACCENT_COLOR, Permissions} from '~/Constants';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {CustomStatusModal} from '~/components/modals/CustomStatusModal';
import {UserProfileModal} from '~/components/modals/UserProfileModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {UserProfileBadges} from '~/components/popouts/UserProfileBadges';
import {UserProfileDataWarning} from '~/components/popouts/UserProfileDataWarning';
import {UserProfileBio, UserProfileMembershipInfo, UserProfileRoles} from '~/components/popouts/UserProfileShared';
import {ProfileCardActions} from '~/components/profile/ProfileCard/ProfileCardActions';
import {ProfileCardBanner} from '~/components/profile/ProfileCard/ProfileCardBanner';
import {ProfileCardContent} from '~/components/profile/ProfileCard/ProfileCardContent';
import {ProfileCardFooter} from '~/components/profile/ProfileCard/ProfileCardFooter';
import {ProfileCardLayout} from '~/components/profile/ProfileCard/ProfileCardLayout';
import {ProfileCardUserInfo} from '~/components/profile/ProfileCard/ProfileCardUserInfo';
import {Button} from '~/components/uikit/Button/Button';
import FocusRingScope from '~/components/uikit/FocusRing/FocusRingScope';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useAutoplayExpandedProfileAnimations} from '~/hooks/useAutoplayExpandedProfileAnimations';
import {useHover} from '~/hooks/useHover';
import type {ProfileRecord} from '~/records/ProfileRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MemberPresenceSubscriptionStore from '~/stores/MemberPresenceSubscriptionStore';
import PermissionStore from '~/stores/PermissionStore';
import UserProfileStore from '~/stores/UserProfileStore';
import * as ColorUtils from '~/utils/ColorUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as ProfileDisplayUtils from '~/utils/ProfileDisplayUtils';
import {createMockProfile} from '~/utils/ProfileUtils';
import styles from './UserProfilePopout.module.css';

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
		const [profile, setProfile] = React.useState<ProfileRecord | null>(() => {
			const cachedProfile = UserProfileStore.getProfile(user.id, guildId);
			return cachedProfile ?? createMockProfile(user);
		});
		const [profileLoadError, setProfileLoadError] = React.useState(false);
		const profileData = React.useMemo(() => profile?.getEffectiveProfile() ?? null, [profile]);

		const guildMember = GuildMemberStore.getMember(profile?.guildId ?? '', user.id);
		const memberRoles = profile?.guildId && guildMember ? guildMember.getSortedRoles() : [];
		const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId});
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;

		const openFullProfile = React.useCallback(
			(autoFocusNote?: boolean) => {
				ModalActionCreators.push(
					modal(() => <UserProfileModal userId={user.id} guildId={guildId} autoFocusNote={autoFocusNote} />),
				);
				PopoutActionCreators.close(popoutKey);
			},
			[user.id, guildId, popoutKey],
		);

		const fetchProfile = React.useCallback(async () => {
			if (isWebhook) return;

			const isGuildMember = guildId ? GuildMemberStore.getMember(guildId, user.id) : false;

			if (DeveloperOptionsStore.slowProfileLoad) {
				await new Promise((resolve) => setTimeout(resolve, 3000));
			}

			setProfileLoadError(false);

			try {
				const fetchedProfile = await UserProfileActionCreators.fetch(user.id, isGuildMember ? guildId : undefined);
				setProfile(fetchedProfile);
				setProfileLoadError(false);
			} catch (error) {
				console.error('Failed to fetch profile for user popout:', error);
				const cachedProfile = UserProfileStore.getProfile(user.id, guildId);
				setProfile(cachedProfile ?? createMockProfile(user));
				setProfileLoadError(true);
			}
		}, [guildId, user.id, isWebhook]);

		React.useEffect(() => {
			fetchProfile();
		}, [fetchProfile]);

		React.useEffect(() => {
			if (!guildId || !user.id || isWebhook) {
				return;
			}

			const hasMember = GuildMemberStore.getMember(guildId, user.id);
			if (!hasMember) {
				MemberPresenceSubscriptionStore.touchMember(guildId, user.id);
				GuildMemberStore.fetchMembers(guildId, {userIds: [user.id]});
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

		const openCustomStatus = React.useCallback(() => {
			ModalActionCreators.push(modal(() => <CustomStatusModal />));
			PopoutActionCreators.close(popoutKey);
		}, [popoutKey]);

		const handleMessage = async () => {
			try {
				PopoutActionCreators.close(popoutKey);
				await PrivateChannelActionCreators.openDMChannel(user.id);
			} catch (error) {
				console.error('Failed to open DM channel:', error);
			}
		};

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

		const {avatarUrl, hoverAvatarUrl} = React.useMemo(
			() => ProfileDisplayUtils.getProfileAvatarUrls(profileContext),
			[profileContext],
		);

		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
		const bannerUrl = React.useMemo(
			() => ProfileDisplayUtils.getProfileBannerUrl(profileContext, undefined, shouldAutoplayProfileAnimations, 600),
			[profileContext, shouldAutoplayProfileAnimations],
		);

		const popoutContainerRef = React.useRef<HTMLDivElement | null>(null);
		const displayName = NicknameUtils.getNickname(user, guildId);

		if (!profile && !isWebhook) {
			return (
				<div className={styles.loadingContainer}>
					<Spinner />
				</div>
			);
		}

		const rawAccentColor = profileData?.accent_color ?? null;
		const accentColorHex = typeof rawAccentColor === 'number' ? ColorUtils.int2hex(rawAccentColor) : rawAccentColor;
		const borderColor = accentColorHex || DEFAULT_ACCENT_COLOR;
		const bannerColor = accentColorHex || DEFAULT_ACCENT_COLOR;

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
							onAvatarClick={() => openFullProfile()}
						/>

						{!isWebhook && (
							<UserProfileBadges
								user={user}
								profile={profile}
								warningIndicator={
									profileLoadError || DeveloperOptionsStore.forceProfileDataWarning ? (
										<UserProfileDataWarning />
									) : undefined
								}
							/>
						)}

						<ProfileCardContent isWebhook={isWebhook}>
							<ProfileCardUserInfo
								displayName={displayName}
								user={user}
								pronouns={profileData?.pronouns}
								showUsername={!isWebhook}
								isClickable={!isWebhook}
								isWebhook={isWebhook}
								onDisplayNameClick={() => openFullProfile()}
								onUsernameClick={() => openFullProfile()}
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
										isEditable={isCurrentUser}
										onEdit={openCustomStatus}
										showPlaceholder={isCurrentUser}
										alwaysAnimate={shouldAutoplayProfileAnimations}
									/>
								</div>
							)}
							{profile && (
								<UserProfileBio
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
										onClick={handleMessage}
									>
										<Trans>Message</Trans>
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
