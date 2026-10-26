// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmailVerificationAlert} from '@app/features/app/components/dialogs/components/EmailVerificationAlert';
import {UnclaimedAccountAlert} from '@app/features/app/components/dialogs/components/UnclaimedAccountAlert';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import Guilds from '@app/features/guild/state/Guilds';
import {dropTrailingEmptyBlockquoteLines} from '@app/features/lexical/composer/blockquoteLines';
import type {LexicalRichInputHandle} from '@app/features/lexical/composer/LexicalRichInput';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {convertMarkdownToSegments} from '@app/features/messaging/utils/MarkdownToSegmentUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {OperatorPassHoldingsSection} from '@app/features/premium/components/operator_pass/OperatorPassHoldingsSection';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {hasOperatorPassProfileBadge, isOperatorPassAvailable} from '@app/features/premium/utils/OperatorPassUtils';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import UnsavedChanges from '@app/features/ui/state/UnsavedChanges';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import styles from '@app/features/user/components/modals/tabs/MyProfileTab.module.css';
import {AccentColorPicker} from '@app/features/user/components/modals/tabs/my_profile_tab/AccentColorPicker';
import {
	AvatarUploader,
	useChangeAvatarAction,
} from '@app/features/user/components/modals/tabs/my_profile_tab/AvatarUploader';
import {BannerUploader} from '@app/features/user/components/modals/tabs/my_profile_tab/BannerUploader';
import {BIO_MARKDOWN_PARSER_FLAGS, BioEditor} from '@app/features/user/components/modals/tabs/my_profile_tab/BioEditor';
import {UsernameSection} from '@app/features/user/components/modals/tabs/my_profile_tab/MyProfileTabUsernameSection';
import {OperatorPassWallSettings} from '@app/features/user/components/modals/tabs/my_profile_tab/OperatorPassWallSettings';
import {PerGuildPremiumUpsell} from '@app/features/user/components/modals/tabs/my_profile_tab/PerGuildPremiumUpsell';
import {
	assignProfileAssetUploadPatch,
	createGlobalProfileAssetRemoteState,
	createProfileAssetCustomizationSnapshot,
	createProfileAssetRemoteStateFromFlags,
	type ProfileAssetCustomizationEvent,
	type ProfileAssetCustomizationSnapshot,
	type ProfileAssetMode,
	type ProfileAssetRemoteState,
	selectProfileAssetCustomizationState,
	transitionProfileAssetCustomizationSnapshot,
} from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileAssetCustomizationStateMachine';
import {
	ProfileBadgeSettings,
	type ProfileBadgeVariantField,
} from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileBadgeSettings';
import {ProfileTypeSelector} from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileTypeSelector';
import {TimezoneProfileSettings} from '@app/features/user/components/modals/tabs/my_profile_tab/TimezoneProfileSettings';
import {ProfilePreview} from '@app/features/user/components/profile/ProfilePreview';
import {PRONOUNS_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {useProfileBadgePreview} from '@app/features/user/hooks/useOwnProfileBadges';
import type {Profile} from '@app/features/user/models/Profile';
import Users from '@app/features/user/state/Users';
import {accountGateReason} from '@app/features/user/utils/AccountGatePresentation';
import {canPerform} from '@app/features/user/utils/AccountGates';
import {ACCOUNT_LIMITED_NOTICE_DESCRIPTOR} from '@app/features/user/utils/AccountLimitUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {
	COMMUNITY_NICKNAME_DESCRIPTOR,
	DISPLAY_NAME_DESCRIPTOR,
	DOC_I_M_FROM_THE_FUTURE_I_CAME_DESCRIPTOR,
	PROFILE_CUSTOMIZATION_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {setMeaningfulFormValue} from '@app/lib/forms/MeaningfulFormValue';
import {type RemoteFormResetReason, useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {AccountTiers, AccountVerificationNeeds, TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {isProfileBadgeKey} from '@fluxer/constants/src/BadgeConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildMemberProfileFlags} from '@fluxer/constants/src/GuildConstants';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {getCurrentTimeZoneOffsetMinutes} from '@fluxer/date_utils/src/TimeZoneUtils';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {UserProfile} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {useForm} from 'react-hook-form';

const COMMUNITY_PROFILE_UPDATED_DESCRIPTOR = msg({
	message: 'Community profile updated',
	comment: 'Short label in the my profile tab. Keep it concise.',
});
const PROFILE_UPDATED_DESCRIPTOR = msg({
	message: 'Profile updated',
	comment: 'Short label in the my profile tab. Keep it concise.',
});
const WARNING_YOU_HAVE_UNSAVED_CHANGES_PLEASE_SAVE_YOUR_DESCRIPTOR = msg({
	message: 'You have unsaved changes. Save or reset before leaving.',
	comment: 'Warning text in the my profile tab. Keep the tone plain and specific.',
});
const VERIFY_YOUR_EMAIL_BEFORE_EDITING_THIS_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Confirm your email before editing this community profile. You can still preview it here.',
	comment: 'Description text in the my profile tab.',
});
const VERIFY_YOUR_EMAIL_BEFORE_EDITING_YOUR_PROFILE_YOU_DESCRIPTOR = msg({
	message: 'Confirm your email before editing your profile. You can still preview it here.',
	comment: 'Description text in the my profile tab.',
});
const EDIT_YOUR_PROFILE_APPEARANCE_AND_SEE_A_LIVE_DESCRIPTOR = msg({
	message: 'Edit your profile appearance and see a live preview',
	comment: 'Button or menu action label in the my profile tab. Keep it concise.',
});
const PROFILE_CUSTOMIZATION_FORM_DESCRIPTOR = msg({
	message: 'Profile customization form',
	comment: 'Short label in the my profile tab. Keep it concise.',
});
const PROFILE_CUSTOMIZATION_FOR_DESCRIPTOR = msg({
	message: 'Profile customization for {selectedGuildName}',
	comment: 'Label in the my profile tab. Preserve {selectedGuildName}; it is inserted by code.',
});

const VERIFY_YOUR_EMAIL_TO_EDIT_THIS_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Confirm your email to edit this community profile',
	comment: 'Label in the my profile tab.',
});
const VERIFY_YOUR_EMAIL_TO_EDIT_YOUR_PROFILE_DESCRIPTOR = msg({
	message: 'Confirm your email to edit your profile',
	comment: 'Label in the my profile tab.',
});
const VERIFY_YOUR_EMAIL_BEFORE_CHANGING_YOUR_COMMUNITY_NICKNAME_DESCRIPTOR = msg({
	message:
		'Confirm your email before changing your community nickname, avatar, banner, bio, pronouns, or accent color.',
	comment: 'Description text in the my profile tab. Keep the tone plain and specific.',
});
const VERIFY_YOUR_EMAIL_BEFORE_CHANGING_YOUR_USERNAME_DISPLAY_DESCRIPTOR = msg({
	message:
		'Confirm your email before changing your username, display name, avatar, banner, bio, pronouns, time zone, or badges.',
	comment: 'Label in the my profile tab.',
});

const THIS_NICKNAME_WILL_ONLY_BE_VISIBLE_IN_THIS_DESCRIPTOR = msg({
	message: 'This nickname will only be visible in this community',
	comment: 'Label in the my profile tab.',
});
const YOU_DON_T_HAVE_PERMISSION_TO_CHANGE_YOUR_DESCRIPTOR = msg({
	message: "You can't change your nickname here",
	comment: 'Label in the my profile tab. Keep the tone plain and specific.',
});

const logger = new Logger('MyProfileTab');

function sameBadgeKeys(a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean {
	return a.length === b.length && a.every((key) => b.includes(key));
}

function hasGuildMemberProfileFlag(member: GuildMemberData, flag: number): boolean {
	return ((member.profile_flags ?? 0) & flag) !== 0;
}

function createProfileAssetEventFromRemote(
	remoteState: ProfileAssetRemoteState,
	reason: RemoteFormResetReason,
): ProfileAssetCustomizationEvent {
	if (reason === 'commit') {
		return {type: 'asset.committed', remoteState};
	}
	return {type: 'asset.remoteApplied', remoteState, force: reason === 'explicit-reset'};
}

function createOptimisticGuildMemberProfile(params: {
	currentProfile: Profile;
	formValues: FormInputs;
	storedAccentColor: number | null;
	updatedMember: GuildMemberData;
}): UserProfile {
	const existingProfile = params.currentProfile.guildMemberProfile;
	return {
		bio: params.formValues.bio ?? null,
		pronouns: params.formValues.pronouns ?? null,
		banner: params.updatedMember.banner ?? null,
		banner_color: existingProfile?.banner_color ?? null,
		accent_color: params.storedAccentColor,
	};
}

interface FormInputs {
	avatar?: string | null;
	banner?: string | null;
	bio: string | null;
	global_name: string | null;
	pronouns: string | null;
	accent_color: number | null;
	timezone: string | null;
	timezone_privacy_flags: number;
	nick?: string | null;
	hidden_profile_badges?: Array<string>;
	premium_badge_timestamp_hidden?: boolean;
	premium_badge_masked?: boolean;
	operator_pass_badge_timestamp_hidden?: boolean;
	operator_pass_badge_count_hidden?: boolean;
}

interface ProfileRemoteValues {
	readonly formValues: FormInputs;
	readonly bioMarkdown: string;
	readonly avatar: ProfileAssetRemoteState;
	readonly banner: ProfileAssetRemoteState;
}

const MY_PROFILE_TAB_ID = 'my_profile';
const MyProfileTabComponent = observer(function MyProfileTabComponent({
	initialGuildId,
}: {
	initialGuildId?: string;
} = {}) {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const unsavedChangesState = UnsavedChanges;
	const mobileLayout = MobileLayout;
	const [selectedGuildId, setSelectedGuildId] = useState<string | null>(initialGuildId || null);
	const [isLoadingProfile, setIsLoadingProfile] = useState(false);
	const [profileData, setProfileData] = useState<Profile | null>(null);
	const activeProfileData =
		profileData?.guildId === selectedGuildId && profileData.userId === user?.id ? profileData : null;
	const cachedGuildMember = GuildMembers.getMember(selectedGuildId || '', user?.id || '');
	const guildMember = activeProfileData?.guildMember ?? cachedGuildMember;
	const [avatarAssetSnapshot, setAvatarAssetSnapshot] = useState<ProfileAssetCustomizationSnapshot>(
		createProfileAssetCustomizationSnapshot,
	);
	const [bannerAssetSnapshot, setBannerAssetSnapshot] = useState<ProfileAssetCustomizationSnapshot>(
		createProfileAssetCustomizationSnapshot,
	);
	const transitionAvatarAsset = useCallback((event: ProfileAssetCustomizationEvent) => {
		setAvatarAssetSnapshot((snapshot) => transitionProfileAssetCustomizationSnapshot(snapshot, event));
	}, []);
	const transitionBannerAsset = useCallback((event: ProfileAssetCustomizationEvent) => {
		setBannerAssetSnapshot((snapshot) => transitionProfileAssetCustomizationSnapshot(snapshot, event));
	}, []);
	const avatarAsset = selectProfileAssetCustomizationState(avatarAssetSnapshot);
	const bannerAsset = selectProfileAssetCustomizationState(bannerAssetSnapshot);
	const bioComposerRef = useRef<LexicalRichInputHandle | null>(null);
	const isPerGuildProfile = selectedGuildId !== null;
	const profileIdentityKey = user?.id ? `${user.id}:${selectedGuildId ?? 'global'}` : null;
	const canChangeNickname = selectedGuildId
		? Permission.can(Permissions.CHANGE_NICKNAME, {guildId: selectedGuildId})
		: false;
	const [bioValue, setBioValue] = useState('');
	const [bioActualValue, setBioActualValue] = useState('');
	const [bioSegments, setBioSegments] = useState<Array<MentionSegment>>([]);
	const [bioHydrationKey, setBioHydrationKey] = useState(0);
	const [isBioInitialized, setIsBioInitialized] = useState(false);
	const originalBioRef = useRef('');
	const originalBioFormValueRef = useRef<string | null>(null);
	const [bioExpressionPickerOpen, setBioExpressionPickerOpen] = useState(false);
	const flashTrigger = unsavedChangesState.flashTriggers[MY_PROFILE_TAB_ID] || 0;
	const [lastFlashTrigger, setLastFlashTrigger] = useState(0);
	const [ariaAnnouncement, setAriaAnnouncement] = useState('');
	const profileGate = canPerform(user, TierGatedActions.PROFILE_CUSTOMIZE);
	const isProfileEmailLocked = !profileGate.allowed && profileGate.need === AccountVerificationNeeds.CONFIRM_EMAIL;
	const isProfileAccountLimited = user?.accountLimited === true;
	const isProfileCustomizationLocked = !profileGate.allowed || isProfileAccountLimited;
	const form = useForm<FormInputs>({
		defaultValues: {
			bio: null,
			global_name: null,
			pronouns: null,
			accent_color: null,
			timezone: user?.timezone ?? null,
			timezone_privacy_flags: user?.timezonePrivacyFlags ?? ProfileFieldPrivacyFlags.EVERYONE,
			nick: null,
			hidden_profile_badges: [...(user?.hiddenProfileBadges ?? [])],
			premium_badge_timestamp_hidden: user?.premiumBadgeTimestampHidden ?? false,
			premium_badge_masked: user?.premiumBadgeMasked ?? false,
			operator_pass_badge_timestamp_hidden: user?.operatorPassBadgeTimestampHidden ?? false,
			operator_pass_badge_count_hidden: user?.operatorPassBadgeCountHidden ?? false,
		},
	});
	const updateBioFromMarkdown = useCallback(
		(markdownBio: string) => {
			const converted = convertMarkdownToSegments(markdownBio, selectedGuildId);
			const segments = converted.segments.map((segment) => ({
				type: segment.type,
				id: segment.id,
				displayText: segment.displayText,
				actualText: segment.actualText,
				start: segment.start,
				end: segment.start + segment.displayText.length,
			}));
			originalBioRef.current = markdownBio;
			setBioValue(converted.displayText);
			setBioActualValue(markdownBio);
			setBioSegments(segments);
			setBioHydrationKey((key) => key + 1);
		},
		[selectedGuildId],
	);
	useEffect(() => {
		if (!user?.id) return;
		if (!selectedGuildId) {
			setProfileData(null);
			setIsLoadingProfile(false);
			return;
		}
		const fetchProfile = async () => {
			setIsLoadingProfile(true);
			try {
				const profile = await UserProfileCommands.fetch(user.id, selectedGuildId);
				setProfileData(profile);
			} catch (error) {
				logger.error('Failed to fetch profile', error);
			} finally {
				setIsLoadingProfile(false);
			}
		};
		fetchProfile();
	}, [selectedGuildId, user?.id]);
	const profileRemoteValues: ProfileRemoteValues | null = (() => {
		if (!user) return null;
		const commonValues = {
			timezone: user.timezone ?? null,
			timezone_privacy_flags: user.timezonePrivacyFlags ?? ProfileFieldPrivacyFlags.EVERYONE,
			hidden_profile_badges: [...(user.hiddenProfileBadges ?? [])],
			premium_badge_timestamp_hidden: user.premiumBadgeTimestampHidden ?? false,
			premium_badge_masked: user.premiumBadgeMasked ?? false,
			operator_pass_badge_timestamp_hidden: user.operatorPassBadgeTimestampHidden ?? false,
			operator_pass_badge_count_hidden: user.operatorPassBadgeCountHidden ?? false,
		};
		if (isPerGuildProfile) {
			if (!activeProfileData?.guildMemberProfile) return null;
			const guildProfile = activeProfileData.guildMemberProfile;
			const markdownBio = guildProfile.bio ?? null;
			const pronouns = guildProfile.pronouns ?? null;
			const accentColor = guildProfile.accent_color !== null ? guildProfile.accent_color : (user.accentColor ?? null);
			const avatarRemoteState = createProfileAssetRemoteStateFromFlags({
				identityKey: profileIdentityKey,
				hasCustomAsset: Boolean(guildMember?.avatar),
				isUnset: guildMember?.isAvatarUnset() ?? false,
			});
			const bannerRemoteState = createProfileAssetRemoteStateFromFlags({
				identityKey: profileIdentityKey,
				hasCustomAsset: Boolean(guildMember?.banner ?? guildProfile.banner),
				isUnset: guildMember?.isBannerUnset() ?? false,
			});
			return {
				formValues: {
					...commonValues,
					bio: markdownBio,
					global_name: null,
					pronouns,
					accent_color: typeof accentColor === 'number' ? accentColor : null,
					nick: guildMember?.nick || null,
				},
				bioMarkdown: markdownBio || '',
				avatar: avatarRemoteState,
				banner: bannerRemoteState,
			};
		}
		const markdownBio = user.bio || null;
		return {
			formValues: {
				...commonValues,
				bio: markdownBio,
				global_name: user.globalName || null,
				pronouns: user.pronouns || null,
				accent_color: typeof user.accentColor === 'number' ? user.accentColor : null,
				nick: null,
			},
			bioMarkdown: markdownBio || '',
			avatar: createGlobalProfileAssetRemoteState({
				identityKey: profileIdentityKey,
				hasCustomAsset: Boolean(user.avatar),
			}),
			banner: createGlobalProfileAssetRemoteState({
				identityKey: profileIdentityKey,
				hasCustomAsset: Boolean(user.banner),
			}),
		};
	})();
	const applyProfileRemoteValues = useCallback(
		(remoteValues: ProfileRemoteValues, reason: RemoteFormResetReason) => {
			originalBioFormValueRef.current = remoteValues.formValues.bio;
			transitionAvatarAsset(createProfileAssetEventFromRemote(remoteValues.avatar, reason));
			transitionBannerAsset(createProfileAssetEventFromRemote(remoteValues.banner, reason));
			setIsBioInitialized(false);
			updateBioFromMarkdown(remoteValues.bioMarkdown);
			setIsBioInitialized(true);
		},
		[transitionAvatarAsset, transitionBannerAsset, updateBioFromMarkdown],
	);
	const isFormDirty = form.formState.isDirty;
	const hasLocalProfileChanges = Boolean(isFormDirty || avatarAsset.isDirty || bannerAsset.isDirty);
	const hasUnsavedChanges = !isProfileCustomizationLocked && hasLocalProfileChanges;
	const {resetToRemoteValues: resetProfileToRemoteValues, commitRemoteValues: commitProfileRemoteValues} =
		useRemoteFormReset<FormInputs, ProfileRemoteValues>({
			form,
			identityKey: profileIdentityKey,
			remoteValues: profileRemoteValues,
			isDirty: hasLocalProfileChanges,
			getFormValues: (remoteValues) => remoteValues.formValues,
			onApply: applyProfileRemoteValues,
		});
	const commitProfileFormValues = useCallback(
		(
			formValues: FormInputs,
			assets: {
				avatar: ProfileAssetRemoteState;
				banner: ProfileAssetRemoteState;
			},
		) => {
			const remoteValues: ProfileRemoteValues = {
				formValues,
				bioMarkdown: formValues.bio || '',
				avatar: assets.avatar,
				banner: assets.banner,
			};
			commitProfileRemoteValues(remoteValues);
		},
		[commitProfileRemoteValues],
	);
	const hasPerGuildProfiles = useMemo(
		() =>
			isLimitToggleEnabled(
				{feature_per_guild_profiles: LimitResolver.resolve({key: 'feature_per_guild_profiles', fallback: 0})},
				'feature_per_guild_profiles',
			),
		[],
	);
	const actualBio = bioActualValue;
	const maxBioActualLength = user?.maxBioLength ?? 0;
	useEffect(() => {
		if (!isBioInitialized) {
			return;
		}
		const isDirty = actualBio.trim() !== originalBioRef.current.trim();
		setMeaningfulFormValue({
			setValue: form.setValue,
			name: 'bio',
			currentValue: actualBio,
			cleanValue: originalBioFormValueRef.current,
			isMeaningfullyDirty: isDirty,
		});
	}, [actualBio, form, isBioInitialized]);
	const handleBioEmojiSelect = useCallback((emoji: FlatEmoji, shiftKey?: boolean) => {
		const composer = bioComposerRef.current;
		if (composer == null) {
			return false;
		}
		const didInsert = composer.insertEmoji(emoji);
		if (didInsert && shiftKey !== true) {
			setBioExpressionPickerOpen(false);
		}
		return didInsert;
	}, []);
	const handleBioChange = useCallback((display: string, segments: Array<MentionSegment>, wire: string) => {
		setBioValue(display);
		setBioSegments(segments);
		setBioActualValue(dropTrailingEmptyBlockquoteLines(wire, BIO_MARKDOWN_PARSER_FLAGS));
	}, []);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			if (isProfileCustomizationLocked) {
				return;
			}
			if (isPerGuildProfile && selectedGuildId && user) {
				const globalAccentColor = typeof user.accentColor === 'number' ? user.accentColor : null;
				const storedAccentColor = data.accent_color === globalAccentColor ? null : data.accent_color;
				let profileFlags = 0;
				if (avatarAsset.mode === 'unset') {
					profileFlags |= GuildMemberProfileFlags.AVATAR_UNSET;
				}
				if (bannerAsset.mode === 'unset') {
					profileFlags |= GuildMemberProfileFlags.BANNER_UNSET;
				}
				const updateData: {
					avatar?: string | null;
					banner?: string | null;
					bio?: string | null;
					pronouns?: string | null;
					accent_color?: number | null;
					nick?: string | null;
					profile_flags?: number | null;
				} = {
					bio: data.bio,
					pronouns: data.pronouns,
					accent_color: storedAccentColor,
					nick: data.nick,
					profile_flags: profileFlags || null,
				};
				assignProfileAssetUploadPatch(updateData, 'avatar', avatarAsset);
				assignProfileAssetUploadPatch(updateData, 'banner', bannerAsset);
				const updatedMember = await GuildMemberCommands.updateProfile(selectedGuildId, updateData);
				if (user?.id) {
					UserProfileCommands.invalidate(user.id, selectedGuildId);
				}
				const savedAvatar = createProfileAssetRemoteStateFromFlags({
					identityKey: profileIdentityKey,
					hasCustomAsset: Boolean(updatedMember.avatar),
					isUnset: hasGuildMemberProfileFlag(updatedMember, GuildMemberProfileFlags.AVATAR_UNSET),
				});
				const savedBanner = createProfileAssetRemoteStateFromFlags({
					identityKey: profileIdentityKey,
					hasCustomAsset: Boolean(updatedMember.banner),
					isUnset: hasGuildMemberProfileFlag(updatedMember, GuildMemberProfileFlags.BANNER_UNSET),
				});
				if (activeProfileData) {
					setProfileData(
						activeProfileData.withUpdates({
							guild_member: updatedMember,
							guild_member_profile: createOptimisticGuildMemberProfile({
								currentProfile: activeProfileData,
								formValues: data,
								storedAccentColor,
								updatedMember,
							}),
						}),
					);
				}
				const savedBio = data.bio || null;
				commitProfileFormValues(
					{
						bio: savedBio,
						global_name: null,
						pronouns: data.pronouns,
						accent_color: data.accent_color,
						timezone: user.timezone ?? null,
						timezone_privacy_flags: user.timezonePrivacyFlags ?? ProfileFieldPrivacyFlags.EVERYONE,
						nick: data.nick,
						hidden_profile_badges: [...(user.hiddenProfileBadges ?? [])],
						premium_badge_timestamp_hidden: user.premiumBadgeTimestampHidden ?? false,
						premium_badge_masked: user.premiumBadgeMasked ?? false,
						operator_pass_badge_timestamp_hidden: user.operatorPassBadgeTimestampHidden ?? false,
						operator_pass_badge_count_hidden: user.operatorPassBadgeCountHidden ?? false,
					},
					{avatar: savedAvatar, banner: savedBanner},
				);
				ToastCommands.createToast({type: 'success', children: i18n._(COMMUNITY_PROFILE_UPDATED_DESCRIPTOR)});
			} else {
				const updateData: Record<string, unknown> = {
					bio: data.bio,
					global_name: data.global_name,
					pronouns: data.pronouns,
					accent_color: data.accent_color,
				};
				assignProfileAssetUploadPatch(updateData, 'avatar', avatarAsset);
				assignProfileAssetUploadPatch(updateData, 'banner', bannerAsset);
				updateData.timezone = data.timezone;
				updateData.timezone_privacy_flags = data.timezone_privacy_flags;
				if (
					data.hidden_profile_badges !== undefined &&
					user?.hiddenProfileBadges !== undefined &&
					!sameBadgeKeys(data.hidden_profile_badges, user.hiddenProfileBadges)
				) {
					updateData.hidden_profile_badges = data.hidden_profile_badges;
				}
				if (data.premium_badge_timestamp_hidden !== undefined) {
					updateData.premium_badge_timestamp_hidden = data.premium_badge_timestamp_hidden;
				}
				if (data.premium_badge_masked !== undefined) {
					updateData.premium_badge_masked = data.premium_badge_masked;
				}
				if (
					data.operator_pass_badge_timestamp_hidden !== undefined &&
					data.operator_pass_badge_timestamp_hidden !== (user?.operatorPassBadgeTimestampHidden ?? false)
				) {
					updateData.operator_pass_badge_timestamp_hidden = data.operator_pass_badge_timestamp_hidden;
				}
				if (
					data.operator_pass_badge_count_hidden !== undefined &&
					data.operator_pass_badge_count_hidden !== (user?.operatorPassBadgeCountHidden ?? false)
				) {
					updateData.operator_pass_badge_count_hidden = data.operator_pass_badge_count_hidden;
				}
				const newUser = await UserCommands.update(updateData);
				UserProfileCommands.clearCurrentUserProfiles();
				const savedBio = newUser.bio || null;
				commitProfileFormValues(
					{
						bio: savedBio,
						global_name: newUser.global_name || null,
						pronouns: newUser.pronouns || null,
						accent_color: typeof newUser.accent_color === 'number' ? newUser.accent_color : null,
						timezone: newUser.timezone ?? null,
						timezone_privacy_flags: newUser.timezone_privacy_flags ?? ProfileFieldPrivacyFlags.EVERYONE,
						nick: null,
						hidden_profile_badges: [...(newUser.hidden_profile_badges ?? user?.hiddenProfileBadges ?? [])],
						premium_badge_timestamp_hidden: newUser.premium_badge_timestamp_hidden ?? false,
						premium_badge_masked: newUser.premium_badge_masked ?? false,
						operator_pass_badge_timestamp_hidden:
							newUser.operator_pass_badge_timestamp_hidden ?? user?.operatorPassBadgeTimestampHidden ?? false,
						operator_pass_badge_count_hidden:
							newUser.operator_pass_badge_count_hidden ?? user?.operatorPassBadgeCountHidden ?? false,
					},
					{
						avatar: createGlobalProfileAssetRemoteState({
							identityKey: profileIdentityKey,
							hasCustomAsset: Boolean(newUser.avatar),
						}),
						banner: createGlobalProfileAssetRemoteState({
							identityKey: profileIdentityKey,
							hasCustomAsset: Boolean(newUser.banner),
						}),
					},
				);
				ToastCommands.createToast({type: 'success', children: i18n._(PROFILE_UPDATED_DESCRIPTOR)});
			}
		},
		[
			commitProfileFormValues,
			isPerGuildProfile,
			isProfileCustomizationLocked,
			selectedGuildId,
			user,
			activeProfileData,
			avatarAsset,
			bannerAsset,
			profileIdentityKey,
		],
	);
	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'bio',
	});
	const handleReset = useCallback(() => {
		resetProfileToRemoteValues();
	}, [resetProfileToRemoteValues]);
	const handleBadgeVariantToggle = useCallback(
		(field: ProfileBadgeVariantField, value: boolean) => {
			form.setValue(field, value, {shouldDirty: true});
		},
		[form],
	);
	const handleBadgeHiddenToggle = useCallback(
		(key: string, hidden: boolean) => {
			const current = form.getValues('hidden_profile_badges') ?? [];
			const next = hidden ? [...new Set([...current, key])] : current.filter((entry) => entry !== key);
			form.setValue('hidden_profile_badges', next, {shouldDirty: true});
		},
		[form],
	);
	const handleAvatarChange = useCallback(
		(base64: string) => {
			form.setValue('avatar', base64);
			transitionAvatarAsset({type: 'asset.uploaded', previewUrl: base64});
			form.clearErrors('avatar');
		},
		[form, transitionAvatarAsset],
	);
	const handleAvatarClear = useCallback(() => {
		form.setValue('avatar', null);
		transitionAvatarAsset({type: 'asset.cleared'});
	}, [form, transitionAvatarAsset]);
	const {openChangeAvatar} = useChangeAvatarAction({onAvatarChange: handleAvatarChange});
	const handleBannerChange = useCallback(
		(base64: string) => {
			form.setValue('banner', base64);
			transitionBannerAsset({type: 'asset.uploaded', previewUrl: base64});
			form.clearErrors('banner');
		},
		[form, transitionBannerAsset],
	);
	const handleBannerClear = useCallback(() => {
		form.setValue('banner', null);
		transitionBannerAsset({type: 'asset.cleared'});
	}, [form, transitionBannerAsset]);
	const handleAvatarModeChange = useCallback(
		(mode: ProfileAssetMode) => {
			transitionAvatarAsset({type: 'asset.modeSelected', mode});
			if (mode === 'inherit' || mode === 'unset') {
				form.setValue('avatar', null);
			}
		},
		[form, transitionAvatarAsset],
	);
	const handleBannerModeChange = useCallback(
		(mode: ProfileAssetMode) => {
			transitionBannerAsset({type: 'asset.modeSelected', mode});
			if (mode === 'inherit' || mode === 'unset') {
				form.setValue('banner', null);
			}
		},
		[form, transitionBannerAsset],
	);
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(MY_PROFILE_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(MY_PROFILE_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [handleReset, handleSave, form.formState.isSubmitting]);
	useEffect(() => {
		if (flashTrigger > lastFlashTrigger) {
			setLastFlashTrigger(flashTrigger);
			setAriaAnnouncement(i18n._(WARNING_YOU_HAVE_UNSAVED_CHANGES_PLEASE_SAVE_YOUR_DESCRIPTOR));
			setTimeout(() => {
				setAriaAnnouncement('');
			}, 1000);
		}
	}, [flashTrigger, lastFlashTrigger]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(MY_PROFILE_TAB_ID);
		};
	}, []);
	const watchedHiddenProfileBadges = form.watch('hidden_profile_badges');
	const premiumBadgeTimestampHidden = form.watch('premium_badge_timestamp_hidden') ?? false;
	const premiumBadgeMasked = form.watch('premium_badge_masked') ?? false;
	const operatorPassBadgeTimestampHidden = form.watch('operator_pass_badge_timestamp_hidden') ?? false;
	const operatorPassBadgeCountHidden = form.watch('operator_pass_badge_count_hidden') ?? false;
	const hasUser = Boolean(user);
	const hiddenProfileBadges = useMemo(() => watchedHiddenProfileBadges ?? [], [watchedHiddenProfileBadges]);
	const badgeVariants = useMemo(
		() => ({
			premium_badge_timestamp_hidden: premiumBadgeTimestampHidden,
			premium_badge_masked: premiumBadgeMasked,
			operator_pass_badge_timestamp_hidden: operatorPassBadgeTimestampHidden,
			operator_pass_badge_count_hidden: operatorPassBadgeCountHidden,
		}),
		[premiumBadgeTimestampHidden, premiumBadgeMasked, operatorPassBadgeTimestampHidden, operatorPassBadgeCountHidden],
	);
	const badgePreviewRequest = useMemo(
		() =>
			hasUser ? {hidden_profile_badges: hiddenProfileBadges.filter(isProfileBadgeKey).sort(), ...badgeVariants} : null,
		[hasUser, hiddenProfileBadges, badgeVariants],
	);
	const badgePreview = useProfileBadgePreview(badgePreviewRequest);
	if (!user) return null;
	const holdsOperatorPasses =
		hasOperatorPassProfileBadge(user) || (OperatorPassHoldings.passes?.passes.length ?? 0) > 0;
	const guilds = Guilds.getGuilds();
	const selectedGuild = selectedGuildId ? guilds.find((g) => g.id === selectedGuildId) : null;
	const isPerGuildProfileCustomizationDisabled = isPerGuildProfile && !hasPerGuildProfiles;
	const isAvatarCustomizationDisabled = isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled;
	const isPronounsDisabled = isProfileCustomizationLocked;
	let profileCustomizationDescription: string;
	if (isProfileEmailLocked) {
		if (isPerGuildProfile) {
			profileCustomizationDescription = i18n._(VERIFY_YOUR_EMAIL_BEFORE_EDITING_THIS_COMMUNITY_PROFILE_DESCRIPTOR);
		} else {
			profileCustomizationDescription = i18n._(VERIFY_YOUR_EMAIL_BEFORE_EDITING_YOUR_PROFILE_YOU_DESCRIPTOR);
		}
	} else if (!profileGate.allowed) {
		profileCustomizationDescription = accountGateReason(i18n, profileGate);
	} else if (isProfileAccountLimited) {
		profileCustomizationDescription = i18n._(ACCOUNT_LIMITED_NOTICE_DESCRIPTOR);
	} else {
		profileCustomizationDescription = i18n._(EDIT_YOUR_PROFILE_APPEARANCE_AND_SEE_A_LIVE_DESCRIPTOR);
	}
	const profileVerificationDescriptor = isPerGuildProfile
		? VERIFY_YOUR_EMAIL_BEFORE_CHANGING_YOUR_COMMUNITY_NICKNAME_DESCRIPTOR
		: VERIFY_YOUR_EMAIL_BEFORE_CHANGING_YOUR_USERNAME_DISPLAY_DESCRIPTOR;
	const hasAvatar =
		!avatarAsset.hasCleared &&
		(avatarAsset.hasAsset || (!avatarAsset.isDirty && Boolean(profileRemoteValues?.avatar.hasCustomAsset)));
	const hasBanner =
		!bannerAsset.hasCleared &&
		(bannerAsset.hasAsset || (!bannerAsset.isDirty && Boolean(profileRemoteValues?.banner.hasCustomAsset)));
	const profileFallbackDisplayName = NicknameUtils.getDisplayName(user);
	const watchedTimezone = form.watch('timezone') ?? null;
	const watchedTimezonePrivacyFlags = form.watch('timezone_privacy_flags') ?? ProfileFieldPrivacyFlags.EVERYONE;
	const previewTimezoneOffset =
		!isPerGuildProfile && watchedTimezone !== null && watchedTimezonePrivacyFlags !== 0
			? getCurrentTimeZoneOffsetMinutes(watchedTimezone)
			: null;
	return (
		<>
			<output
				aria-live="assertive"
				aria-atomic="true"
				className={styles.srOnly}
				data-flx="user.my-profile-tab.my-profile-tab-component.sr-only"
			>
				{ariaAnnouncement}
			</output>
			<SettingsTabContainer data-flx="user.my-profile-tab.my-profile-tab-component.settings-tab-container">
				{profileGate.tier === AccountTiers.UNCLAIMED && (
					<UnclaimedAccountAlert data-flx="user.my-profile-tab.my-profile-tab-component.unclaimed-account-alert" />
				)}
				<Form
					form={form}
					onSubmit={onSubmit}
					aria-label={i18n._(PROFILE_CUSTOMIZATION_FORM_DESCRIPTOR)}
					data-flx="user.my-profile-tab.my-profile-tab-component.form.submit"
				>
					<ProfileTypeSelector
						selectedGuildId={selectedGuildId}
						onChange={setSelectedGuildId}
						disabled={hasUnsavedChanges}
						data-flx="user.my-profile-tab.my-profile-tab-component.profile-type-selector.set-selected-guild-id"
					/>
					<SettingsSection
						id="profile-customization"
						title={
							isPerGuildProfile && selectedGuild
								? i18n._(PROFILE_CUSTOMIZATION_FOR_DESCRIPTOR, {selectedGuildName: selectedGuild.name || ''})
								: i18n._(PROFILE_CUSTOMIZATION_DESCRIPTOR)
						}
						description={profileCustomizationDescription}
						data-flx="user.my-profile-tab.my-profile-tab-component.settings-section"
					>
						{isProfileEmailLocked && (
							<EmailVerificationAlert
								title={
									isPerGuildProfile
										? i18n._(VERIFY_YOUR_EMAIL_TO_EDIT_THIS_COMMUNITY_PROFILE_DESCRIPTOR)
										: i18n._(VERIFY_YOUR_EMAIL_TO_EDIT_YOUR_PROFILE_DESCRIPTOR)
								}
								data-flx="user.my-profile-tab.my-profile-tab-component.email-verification-alert"
							>
								{i18n._(profileVerificationDescriptor)}
							</EmailVerificationAlert>
						)}
						{isLoadingProfile ? (
							<div
								className={styles.loadingContainer}
								data-flx="user.my-profile-tab.my-profile-tab-component.loading-container"
							>
								<Spinner data-flx="user.my-profile-tab.my-profile-tab-component.spinner" />
							</div>
						) : (
							<div
								className={styles.contentLayout}
								data-flx="user.my-profile-tab.my-profile-tab-component.content-layout"
							>
								<div className={styles.formColumn} data-flx="user.my-profile-tab.my-profile-tab-component.form-column">
									{!isPerGuildProfile && (
										<UsernameSection
											profileGate={profileGate}
											user={user}
											data-flx="user.my-profile-tab.my-profile-tab-component.username-section"
										/>
									)}
									{isPerGuildProfile && (
										<div data-flx="user.my-profile-tab.my-profile-tab-component.div">
											<Input
												data-flx="user.my-profile-tab.my-profile-tab-component.input"
												{...form.register('nick')}
												label={i18n._(COMMUNITY_NICKNAME_DESCRIPTOR)}
												placeholder={profileFallbackDisplayName}
												maxLength={32}
												value={form.watch('nick') || ''}
												footer={
													<div
														className={styles.inputFooter}
														data-flx="user.my-profile-tab.my-profile-tab-component.input-footer"
													>
														{canChangeNickname
															? i18n._(THIS_NICKNAME_WILL_ONLY_BE_VISIBLE_IN_THIS_DESCRIPTOR)
															: i18n._(YOU_DON_T_HAVE_PERMISSION_TO_CHANGE_YOUR_DESCRIPTOR)}
													</div>
												}
												disabled={isProfileCustomizationLocked || !canChangeNickname}
											/>
										</div>
									)}
									{!isPerGuildProfile && (
										<div data-flx="user.my-profile-tab.my-profile-tab-component.div--2">
											<Input
												data-flx="user.my-profile-tab.my-profile-tab-component.input--2"
												{...form.register('global_name')}
												label={i18n._(DISPLAY_NAME_DESCRIPTOR)}
												placeholder={user.username}
												maxLength={32}
												value={form.watch('global_name') || ''}
												error={form.formState.errors.global_name?.message}
												disabled={isProfileCustomizationLocked}
											/>
										</div>
									)}
									<div data-flx="user.my-profile-tab.my-profile-tab-component.div--3">
										<Input
											data-flx="user.my-profile-tab.my-profile-tab-component.input--3"
											{...form.register('pronouns')}
											label={i18n._(PRONOUNS_DESCRIPTOR)}
											maxLength={40}
											value={form.watch('pronouns') || ''}
											error={form.formState.errors.pronouns?.message}
											disabled={isPronounsDisabled}
										/>
									</div>
									{!isPerGuildProfile && (
										<TimezoneProfileSettings
											timezone={watchedTimezone}
											timezonePrivacyFlags={watchedTimezonePrivacyFlags}
											disabled={isProfileCustomizationLocked}
											onTimezoneChange={(value) => {
												form.setValue('timezone', value, {shouldDirty: true});
											}}
											onTimezonePrivacyFlagsChange={(value) =>
												form.setValue('timezone_privacy_flags', value, {shouldDirty: true})
											}
											data-flx="user.my-profile-tab.my-profile-tab-component.timezone-profile-settings"
										/>
									)}
									{isPerGuildProfile && !hasPerGuildProfiles && (
										<PerGuildPremiumUpsell data-flx="user.my-profile-tab.my-profile-tab-component.per-guild-premium-upsell" />
									)}
									<div data-flx="user.my-profile-tab.my-profile-tab-component.div--4">
										<AvatarUploader
											hasAvatar={hasAvatar}
											onAvatarChange={handleAvatarChange}
											onAvatarClear={handleAvatarClear}
											disabled={isAvatarCustomizationDisabled}
											disableModeSelection={isProfileCustomizationLocked}
											isPerGuildProfile={isPerGuildProfile}
											errorMessage={form.formState.errors.avatar?.message}
											avatarMode={avatarAsset.mode}
											onAvatarModeChange={handleAvatarModeChange}
											data-flx="user.my-profile-tab.my-profile-tab-component.avatar-uploader"
										/>
									</div>
									<div data-flx="user.my-profile-tab.my-profile-tab-component.div--5">
										<BannerUploader
											hasBanner={hasBanner}
											onBannerChange={handleBannerChange}
											onBannerClear={handleBannerClear}
											disabled={isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled}
											disableModeSelection={isProfileCustomizationLocked}
											hideUploadWhenMissingEntitlement={true}
											isPerGuildProfile={isPerGuildProfile}
											errorMessage={form.formState.errors.banner?.message}
											bannerMode={bannerAsset.mode}
											onBannerModeChange={handleBannerModeChange}
											data-flx="user.my-profile-tab.my-profile-tab-component.banner-uploader"
										/>
									</div>
									<div
										className={isPerGuildProfile && !hasPerGuildProfiles ? styles.opacityHalf : ''}
										data-flx="user.my-profile-tab.my-profile-tab-component.opacity-half"
									>
										<AccentColorPicker
											value={form.watch('accent_color') ?? null}
											onChange={(value: number | null) => form.setValue('accent_color', value, {shouldDirty: true})}
											disabled={isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled}
											errorMessage={form.formState.errors.accent_color?.message}
											data-flx="user.my-profile-tab.my-profile-tab-component.accent-color-picker.set-value"
										/>
									</div>
									<div
										className={isPerGuildProfile && !hasPerGuildProfiles ? styles.opacityHalf : ''}
										data-flx="user.my-profile-tab.my-profile-tab-component.opacity-half--2"
									>
										<BioEditor
											initialValue={bioValue}
											initialSegments={bioSegments}
											hydrationKey={bioHydrationKey}
											onChange={handleBioChange}
											onEmojiSelect={handleBioEmojiSelect}
											placeholder={
												isPerGuildProfile && user?.bio
													? convertMarkdownToSegments(user.bio, selectedGuildId).displayText
													: i18n._(DOC_I_M_FROM_THE_FUTURE_I_CAME_DESCRIPTOR)
											}
											actualLength={actualBio.length}
											actualMaxLength={maxBioActualLength}
											disabled={isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled}
											isMobile={mobileLayout.enabled}
											errorMessage={
												form.formState.errors.bio != null && form.formState.errors.bio.message != null
													? form.formState.errors.bio.message
													: null
											}
											composerRef={bioComposerRef}
											emojiPickerOpen={bioExpressionPickerOpen}
											onEmojiPickerOpenChange={setBioExpressionPickerOpen}
											data-flx="user.my-profile-tab.my-profile-tab-component.bio-editor.bio-change"
										/>
									</div>
								</div>
								<div
									className={styles.previewColumn}
									data-flx="user.my-profile-tab.my-profile-tab-component.preview-column"
								>
									<ProfilePreview
										user={user}
										previewAvatarUrl={avatarAsset.previewUrl}
										previewBannerUrl={bannerAsset.previewUrl}
										hasClearedAvatar={avatarAsset.hasCleared}
										hasClearedBanner={bannerAsset.hasCleared}
										previewBio={actualBio}
										previewPronouns={form.watch('pronouns')}
										previewAccentColor={form.watch('accent_color')}
										previewTimezoneOffset={previewTimezoneOffset}
										previewGlobalName={!isPerGuildProfile ? form.watch('global_name') : undefined}
										previewNick={isPerGuildProfile ? form.watch('nick') : undefined}
										guildId={selectedGuildId}
										guildMember={isPerGuildProfile ? guildMember : undefined}
										guildMemberProfile={isPerGuildProfile ? activeProfileData?.guildMemberProfile : undefined}
										previewProfileBadges={badgePreview.value?.profile_badges}
										ignoreGuildAvatarInPreview={isPerGuildProfile && avatarAsset.mode === 'inherit'}
										ignoreGuildBannerInPreview={isPerGuildProfile && bannerAsset.mode === 'inherit'}
										onAvatarClick={
											mobileLayout.enabled && !isAvatarCustomizationDisabled ? openChangeAvatar : undefined
										}
										data-flx="user.my-profile-tab.my-profile-tab-component.profile-preview"
									/>
								</div>
							</div>
						)}
					</SettingsSection>
					{!isPerGuildProfile && (
						<ProfileBadgeSettings
							preview={badgePreview}
							hiddenProfileBadges={hiddenProfileBadges}
							variants={badgeVariants}
							disabled={isProfileCustomizationLocked}
							onToggleHidden={handleBadgeHiddenToggle}
							onToggleVariant={handleBadgeVariantToggle}
							data-flx="user.my-profile-tab.my-profile-tab-component.profile-badge-settings"
						/>
					)}
					{isOperatorPassAvailable() && !isPerGuildProfile && holdsOperatorPasses && (
						<OperatorPassWallSettings data-flx="user.my-profile-tab.my-profile-tab-component.operator-pass-wall-settings" />
					)}
					{isOperatorPassAvailable() && !isPerGuildProfile && (
						<OperatorPassHoldingsSection data-flx="user.my-profile-tab.my-profile-tab-component.operator-pass-holdings" />
					)}
				</Form>
			</SettingsTabContainer>
			{mobileLayout.enabled && (
				<ExpressionPickerSheet
					isOpen={bioExpressionPickerOpen}
					onClose={() => setBioExpressionPickerOpen(false)}
					onEmojiSelect={handleBioEmojiSelect}
					visibleTabs={['emojis']}
					zIndex={30000}
					data-flx="user.my-profile-tab.my-profile-tab-component.expression-picker-sheet"
				/>
			)}
		</>
	);
});

export default MyProfileTabComponent;
