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
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {GuildMemberProfileFlags, UserPremiumTypes} from '~/Constants';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import {ExpressionPickerSheet} from '~/components/modals/ExpressionPickerSheet';
import {
	SettingsTabContainer,
	SettingsTabHeader,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {ProfilePreview} from '~/components/profile/ProfilePreview';
import {Spinner} from '~/components/uikit/Spinner';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import {useTextareaAutocomplete} from '~/hooks/useTextareaAutocomplete';
import {useTextareaEmojiPicker} from '~/hooks/useTextareaEmojiPicker';
import {useTextareaPaste} from '~/hooks/useTextareaPaste';
import {useTextareaSegments} from '~/hooks/useTextareaSegments';
import type {ProfileRecord} from '~/records/ProfileRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UnsavedChangesStore from '~/stores/UnsavedChangesStore';
import UserStore from '~/stores/UserStore';
import {applyMarkdownSegments, convertMarkdownToSegments} from '~/utils/MarkdownToSegmentUtils';
import {UnclaimedAccountAlert} from '../components/UnclaimedAccountAlert';
import {AccentColorPicker} from './MyProfileTab/AccentColorPicker';
import {type AvatarMode, AvatarUploader} from './MyProfileTab/AvatarUploader';
import {type BannerMode, BannerUploader} from './MyProfileTab/BannerUploader';
import {BioEditor} from './MyProfileTab/BioEditor';
import {PerGuildPremiumUpsell} from './MyProfileTab/PerGuildPremiumUpsell';
import {PremiumBadgeSettings} from './MyProfileTab/PremiumBadgeSettings';
import {ProfileTypeSelector} from './MyProfileTab/ProfileTypeSelector';
import {UsernameSection} from './MyProfileTab/UsernameSection';
import styles from './MyProfileTab.module.css';

interface FormInputs {
	avatar?: string | null;
	banner?: string | null;
	bio: string | null;
	global_name: string | null;
	pronouns: string | null;
	accent_color: number | null;
	nick?: string | null;
	premium_badge_hidden?: boolean;
	premium_badge_timestamp_hidden?: boolean;
	premium_badge_masked?: boolean;
	premium_badge_sequence_hidden?: boolean;
}

const MY_PROFILE_TAB_ID = 'my_profile';
const AUTOCOMPLETE_Z_INDEX = 10001;

const convertNumberToHexColor = (color: number | string | null | undefined): string | null => {
	if (color === null || color === undefined) return null;
	if (color === 0) return null;
	if (typeof color === 'string') return color;
	return `#${color.toString(16).padStart(6, '0')}`;
};

const MyProfileTabComponent = observer(function MyProfileTabComponent({
	initialGuildId,
}: {
	initialGuildId?: string;
} = {}) {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const unsavedChangesStore = UnsavedChangesStore;
	const mobileLayout = MobileLayoutStore;
	const [selectedGuildId, setSelectedGuildId] = React.useState<string | null>(initialGuildId || null);
	const guildMember = GuildMemberStore.getMember(selectedGuildId || '', user?.id || '');
	const [isLoadingProfile, setIsLoadingProfile] = React.useState(false);
	const [profileData, setProfileData] = React.useState<ProfileRecord | null>(null);
	const [hasClearedAvatar, setHasClearedAvatar] = React.useState(false);
	const [previewAvatarUrl, setPreviewAvatarUrl] = React.useState<string | null>(null);
	const [hasClearedBanner, setHasClearedBanner] = React.useState(false);
	const [previewBannerUrl, setPreviewBannerUrl] = React.useState<string | null>(null);
	const bioTextareaRef = React.useRef<HTMLTextAreaElement | null>(null);

	const isPerGuildProfile = selectedGuildId !== null;

	const {segmentManagerRef, previousValueRef, displayToActual, insertSegment, handleTextChange} = useTextareaSegments();

	const [bioValue, setBioValue] = React.useState('');
	const [isBioInitialized, setIsBioInitialized] = React.useState(false);
	const originalBioRef = React.useRef('');
	const [hasCustomAvatar, setHasCustomAvatar] = React.useState(false);
	const [hasCustomBanner, setHasCustomBanner] = React.useState(false);
	const [avatarMode, setAvatarMode] = React.useState<AvatarMode>('inherit');
	const [bannerMode, setBannerMode] = React.useState<BannerMode>('inherit');
	const [initialAvatarMode, setInitialAvatarMode] = React.useState<AvatarMode>('inherit');
	const [initialBannerMode, setInitialBannerMode] = React.useState<BannerMode>('inherit');

	const {handleEmojiSelect} = useTextareaEmojiPicker({
		setValue: setBioValue,
		textareaRef: bioTextareaRef,
		insertSegment,
		previousValueRef,
	});

	const {
		autocompleteQuery,
		autocompleteOptions,
		autocompleteType,
		selectedIndex,
		isAutocompleteAttached,
		setSelectedIndex,
		onCursorMove,
		handleSelect,
	} = useTextareaAutocomplete({
		channel: null,
		value: bioValue,
		setValue: setBioValue,
		textareaRef: bioTextareaRef,
		segmentManagerRef,
		previousValueRef,
		allowedTriggers: ['emoji'],
	});

	useTextareaPaste({
		channel: null,
		textareaRef: bioTextareaRef,
		segmentManagerRef,
		setValue: setBioValue,
		previousValueRef,
	});

	const [bioExpressionPickerOpen, setBioExpressionPickerOpen] = React.useState(false);
	const bioContainerRef = React.useRef<HTMLDivElement | null>(null);

	const flashTrigger = unsavedChangesStore.flashTriggers[MY_PROFILE_TAB_ID] || 0;
	const [lastFlashTrigger, setLastFlashTrigger] = React.useState(0);
	const [ariaAnnouncement, setAriaAnnouncement] = React.useState('');

	const form = useForm<FormInputs>({
		defaultValues: {
			bio: null,
			global_name: null,
			pronouns: null,
			accent_color: null,
			nick: null,
			premium_badge_hidden: false,
			premium_badge_timestamp_hidden: false,
			premium_badge_masked: false,
			premium_badge_sequence_hidden: false,
		},
	});

	const updateBioFromMarkdown = React.useCallback(
		(markdownBio: string) => {
			segmentManagerRef.current.clear();
			const displayBio = markdownBio
				? applyMarkdownSegments(markdownBio, selectedGuildId, segmentManagerRef.current)
				: '';
			originalBioRef.current = markdownBio;
			setBioValue(displayBio);
			previousValueRef.current = displayBio;
		},
		[selectedGuildId, segmentManagerRef, previousValueRef],
	);

	React.useEffect(() => {
		if (!user?.id) return;

		if (!selectedGuildId) {
			setProfileData(null);
			setIsLoadingProfile(false);
			return;
		}

		const fetchProfile = async () => {
			setIsLoadingProfile(true);
			try {
				const profile = await UserProfileActionCreators.fetch(user.id, selectedGuildId);
				setProfileData(profile);
			} catch (error) {
				console.error('Failed to fetch profile:', error);
			} finally {
				setIsLoadingProfile(false);
			}
		};

		fetchProfile();
	}, [selectedGuildId, user?.id]);

	React.useLayoutEffect(() => {
		if (isPerGuildProfile && profileData?.guildMemberProfile && user) {
			const guildProfile = profileData.guildMemberProfile;
			const markdownBio = guildProfile.bio ?? null;
			const pronouns = guildProfile.pronouns ?? null;
			const accentColor = guildProfile.accent_color !== null ? guildProfile.accent_color : (user.accentColor ?? null);

			form.reset({
				bio: markdownBio,
				pronouns: pronouns,
				accent_color: typeof accentColor === 'number' ? accentColor : null,
				nick: guildMember?.nick || null,
			});

			setHasCustomAvatar(guildMember?.avatar !== null && !guildMember?.isAvatarUnset());
			setHasCustomBanner(guildProfile.banner !== null && !guildMember?.isBannerUnset());

			let computedAvatarMode: AvatarMode = 'inherit';
			if (guildMember?.isAvatarUnset()) {
				computedAvatarMode = 'unset';
			} else if (guildMember?.avatar) {
				computedAvatarMode = 'custom';
			}
			setAvatarMode(computedAvatarMode);
			setInitialAvatarMode(computedAvatarMode);

			let computedBannerMode: BannerMode = 'inherit';
			if (guildMember?.isBannerUnset()) {
				computedBannerMode = 'unset';
			} else if (guildProfile.banner) {
				computedBannerMode = 'custom';
			}
			setBannerMode(computedBannerMode);
			setInitialBannerMode(computedBannerMode);

			setIsBioInitialized(false);
			updateBioFromMarkdown(markdownBio || '');
			setIsBioInitialized(true);
		} else if (!isPerGuildProfile && user) {
			const markdownBio = user.bio || null;

			form.reset({
				bio: markdownBio,
				global_name: user.globalName || null,
				pronouns: user.pronouns || null,
				accent_color: typeof user.accentColor === 'number' ? user.accentColor : null,
				nick: null,
				premium_badge_hidden: user.premiumBadgeHidden ?? false,
				premium_badge_timestamp_hidden: user.premiumBadgeTimestampHidden ?? false,
				premium_badge_masked: user.premiumBadgeMasked ?? false,
				premium_badge_sequence_hidden: user.premiumBadgeSequenceHidden ?? false,
			});

			setHasCustomAvatar(false);
			setHasCustomBanner(false);
			setAvatarMode('inherit');
			setBannerMode('inherit');
			setInitialAvatarMode('inherit');
			setInitialBannerMode('inherit');

			setIsBioInitialized(false);
			updateBioFromMarkdown(markdownBio || '');
			setIsBioInitialized(true);
		}

		setHasClearedAvatar(false);
		setPreviewAvatarUrl(null);
		setHasClearedBanner(false);
		setPreviewBannerUrl(null);
	}, [isPerGuildProfile, guildMember, profileData, user, form, updateBioFromMarkdown]);

	const isFormDirty = form.formState.isDirty;
	const hasModeChanges = isPerGuildProfile && (avatarMode !== initialAvatarMode || bannerMode !== initialBannerMode);
	const hasUnsavedChanges = Boolean(
		isFormDirty || previewAvatarUrl || hasClearedAvatar || previewBannerUrl || hasClearedBanner || hasModeChanges,
	);

	const hasPremium = React.useMemo(() => user?.isPremium() ?? false, [user]);

	React.useEffect(() => {
		if (!isBioInitialized) {
			return;
		}

		const actualBio = displayToActual(bioValue);
		const isDirty = actualBio.trim() !== originalBioRef.current.trim();

		form.setValue('bio', actualBio, {shouldDirty: isDirty, shouldTouch: false});

		if (!isDirty && form.formState.dirtyFields.bio) {
			const currentValues = form.getValues();
			form.reset({...currentValues, bio: originalBioRef.current}, {keepValues: true});
		}
	}, [bioValue, displayToActual, form, isBioInitialized]);

	const handleBioEmojiSelect = React.useCallback(
		(emoji: any) => {
			handleEmojiSelect(emoji);
			setBioExpressionPickerOpen(false);
		},
		[handleEmojiSelect],
	);

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			if (isPerGuildProfile && selectedGuildId && user) {
				const globalAccentColor = typeof user.accentColor === 'number' ? user.accentColor : null;

				let profileFlags = 0;
				if (avatarMode === 'unset') {
					profileFlags |= GuildMemberProfileFlags.AVATAR_UNSET;
				}
				if (bannerMode === 'unset') {
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
					avatar: avatarMode === 'inherit' || avatarMode === 'unset' ? null : data.avatar,
					banner: bannerMode === 'inherit' || bannerMode === 'unset' ? null : data.banner,
					bio: data.bio,
					pronouns: data.pronouns,
					accent_color: data.accent_color === globalAccentColor ? null : data.accent_color,
					nick: data.nick,
					profile_flags: profileFlags || null,
				};

				await GuildMemberActionCreators.updateProfile(selectedGuildId, updateData);

				if (user?.id) {
					UserProfileActionCreators.invalidate(user.id, selectedGuildId);
				}

				const savedBio = data.bio || null;
				updateBioFromMarkdown(savedBio || '');

				form.reset({
					bio: savedBio,
					pronouns: data.pronouns,
					accent_color: data.accent_color,
					nick: data.nick,
				});
				setInitialAvatarMode(avatarMode);
				setInitialBannerMode(bannerMode);
				ToastActionCreators.createToast({type: 'success', children: t`Community profile updated`});
				if (user?.id) {
					const updatedProfile = await UserProfileActionCreators.fetch(user.id, selectedGuildId, true);
					setProfileData(updatedProfile);
				}
			} else {
				const updateData: Record<string, unknown> = {
					avatar: data.avatar,
					banner: data.banner,
					bio: data.bio,
					global_name: data.global_name,
					pronouns: data.pronouns,
					accent_color: data.accent_color,
				};

				if (data.premium_badge_hidden !== undefined) {
					updateData.premium_badge_hidden = data.premium_badge_hidden;
				}
				if (data.premium_badge_timestamp_hidden !== undefined) {
					updateData.premium_badge_timestamp_hidden = data.premium_badge_timestamp_hidden;
				}
				if (data.premium_badge_masked !== undefined) {
					updateData.premium_badge_masked = data.premium_badge_masked;
					if (data.premium_badge_masked) {
						updateData.premium_badge_sequence_hidden = true;
					}
				}
				if (data.premium_badge_sequence_hidden !== undefined && !data.premium_badge_masked) {
					updateData.premium_badge_sequence_hidden = data.premium_badge_sequence_hidden;
				}

				const newUser = await UserActionCreators.update(updateData);

				UserProfileActionCreators.clearCurrentUserProfiles();

				const savedBio = newUser.bio || null;
				updateBioFromMarkdown(savedBio || '');

				form.reset({
					bio: savedBio,
					global_name: newUser.global_name || null,
					pronouns: newUser.pronouns || null,
					accent_color: typeof newUser.accent_color === 'number' ? newUser.accent_color : null,
					premium_badge_hidden: newUser.premium_badge_hidden ?? false,
					premium_badge_timestamp_hidden: newUser.premium_badge_timestamp_hidden ?? false,
					premium_badge_masked: newUser.premium_badge_masked ?? false,
					premium_badge_sequence_hidden: newUser.premium_badge_sequence_hidden ?? false,
				});
				ToastActionCreators.createToast({type: 'success', children: t`Profile updated`});
			}

			setPreviewAvatarUrl(null);
			setHasClearedAvatar(false);
			setPreviewBannerUrl(null);
			setHasClearedBanner(false);
		},
		[form, isPerGuildProfile, selectedGuildId, updateBioFromMarkdown, user, avatarMode, bannerMode],
	);

	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'bio',
	});

	const handleReset = React.useCallback(() => {
		if (isPerGuildProfile && profileData?.guildMemberProfile && user) {
			const guildProfile = profileData.guildMemberProfile;
			const markdownBio = guildProfile.bio ?? null;
			const pronouns = guildProfile.pronouns ?? null;
			const accentColor = guildProfile.accent_color !== null ? guildProfile.accent_color : (user.accentColor ?? null);

			form.reset({
				bio: markdownBio,
				pronouns: pronouns,
				accent_color: typeof accentColor === 'number' ? accentColor : null,
				nick: guildMember?.nick || null,
			});

			setHasCustomAvatar(guildMember?.avatar !== null && !guildMember?.isAvatarUnset());
			setHasCustomBanner(guildProfile.banner !== null && !guildMember?.isBannerUnset());
			setAvatarMode(initialAvatarMode);
			setBannerMode(initialBannerMode);

			updateBioFromMarkdown(markdownBio || '');
		} else if (user) {
			const markdownBio = user.bio || null;

			form.reset({
				bio: markdownBio,
				global_name: user.globalName || null,
				pronouns: user.pronouns || null,
				accent_color: typeof user.accentColor === 'number' ? user.accentColor : null,
				nick: null,
				premium_badge_hidden: user.premiumBadgeHidden ?? false,
				premium_badge_timestamp_hidden: user.premiumBadgeTimestampHidden ?? false,
				premium_badge_masked: user.premiumBadgeMasked ?? false,
				premium_badge_sequence_hidden: user.premiumBadgeSequenceHidden ?? false,
			});

			setHasCustomAvatar(false);
			setHasCustomBanner(false);
			setAvatarMode('inherit');
			setBannerMode('inherit');

			updateBioFromMarkdown(markdownBio || '');
		}

		setHasClearedAvatar(false);
		setPreviewAvatarUrl(null);
		setHasClearedBanner(false);
		setPreviewBannerUrl(null);
	}, [
		form,
		user,
		isPerGuildProfile,
		profileData,
		guildMember,
		updateBioFromMarkdown,
		initialAvatarMode,
		initialBannerMode,
	]);

	const handlePremiumBadgeToggle = React.useCallback(
		(field: keyof FormInputs, value: boolean) => {
			form.setValue(field, value, {shouldDirty: true});
			if (field === 'premium_badge_masked' && value) {
				form.setValue('premium_badge_sequence_hidden', true, {shouldDirty: true});
			}
		},
		[form],
	);

	const handleAvatarChange = React.useCallback(
		(base64: string) => {
			form.setValue('avatar', base64);
			setPreviewAvatarUrl(base64);
			setHasClearedAvatar(false);
			if (isPerGuildProfile) {
				setHasCustomAvatar(true);
				setAvatarMode('custom');
			}
			form.clearErrors('avatar');
		},
		[form, isPerGuildProfile],
	);

	const handleAvatarClear = React.useCallback(() => {
		form.setValue('avatar', null);
		setPreviewAvatarUrl(null);
		setHasClearedAvatar(true);
		if (isPerGuildProfile) {
			setHasCustomAvatar(false);
		}
	}, [form, isPerGuildProfile]);

	const handleBannerChange = React.useCallback(
		(base64: string) => {
			form.setValue('banner', base64);
			setPreviewBannerUrl(base64);
			setHasClearedBanner(false);
			if (isPerGuildProfile) {
				setHasCustomBanner(true);
				setBannerMode('custom');
			}
			form.clearErrors('banner');
		},
		[form, isPerGuildProfile],
	);

	const handleBannerClear = React.useCallback(() => {
		form.setValue('banner', null);
		setPreviewBannerUrl(null);
		setHasClearedBanner(true);
		if (isPerGuildProfile) {
			setHasCustomBanner(false);
		}
	}, [form, isPerGuildProfile]);

	const handleAvatarModeChange = React.useCallback(
		(mode: AvatarMode) => {
			setAvatarMode(mode);
			if (mode === 'inherit' || mode === 'unset') {
				setHasCustomAvatar(false);
				setPreviewAvatarUrl(null);
				form.setValue('avatar', null);
			}
			if (mode === 'unset') {
				setHasClearedAvatar(true);
			} else {
				setHasClearedAvatar(false);
			}
		},
		[form],
	);

	const handleBannerModeChange = React.useCallback(
		(mode: BannerMode) => {
			setBannerMode(mode);
			if (mode === 'inherit' || mode === 'unset') {
				setHasCustomBanner(false);
				setPreviewBannerUrl(null);
				form.setValue('banner', null);
			}
			if (mode === 'unset') {
				setHasClearedBanner(true);
			} else {
				setHasClearedBanner(false);
			}
		},
		[form],
	);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(MY_PROFILE_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(MY_PROFILE_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [handleReset, handleSave, form.formState.isSubmitting]);

	React.useEffect(() => {
		if (flashTrigger > lastFlashTrigger) {
			setLastFlashTrigger(flashTrigger);
			setAriaAnnouncement(
				t`Warning: You have unsaved changes. Please save your changes or reset the form before leaving.`,
			);
			setTimeout(() => {
				setAriaAnnouncement('');
			}, 1000);
		}
	}, [flashTrigger, lastFlashTrigger]);

	React.useEffect(() => {
		return () => {
			UnsavedChangesActionCreators.clearUnsavedChanges(MY_PROFILE_TAB_ID);
		};
	}, []);

	if (!user) return null;

	const hasLifetimePremium = user.premiumType === UserPremiumTypes.LIFETIME;
	const guilds = GuildStore.getGuilds();
	const selectedGuild = selectedGuildId ? guilds.find((g) => g.id === selectedGuildId) : null;

	const isClaimed = user.isClaimed();

	const hasAvatar = isPerGuildProfile
		? hasCustomAvatar || Boolean(previewAvatarUrl)
		: Boolean(user.avatar) || Boolean(previewAvatarUrl);
	const hasBanner = isPerGuildProfile
		? hasCustomBanner || Boolean(previewBannerUrl)
		: Boolean(user.banner) || Boolean(previewBannerUrl);

	return (
		<>
			<output aria-live="assertive" aria-atomic="true" className={styles.srOnly}>
				{ariaAnnouncement}
			</output>
			<SettingsTabContainer>
				{!isClaimed && <UnclaimedAccountAlert />}
				<Form form={form} onSubmit={onSubmit} aria-label={t`Profile customization form`}>
					<ProfileTypeSelector
						selectedGuildId={selectedGuildId}
						onChange={setSelectedGuildId}
						disabled={hasUnsavedChanges}
					/>

					<SettingsTabSection>
						<SettingsTabHeader
							title={
								isPerGuildProfile && selectedGuild
									? t`Profile Customization for ${selectedGuild.name || ''}`
									: t`Profile Customization`
							}
							description={t`Edit your profile appearance and see a live preview`}
						/>
						{isLoadingProfile ? (
							<div className={styles.loadingContainer}>
								<Spinner />
							</div>
						) : (
							<div className={styles.contentLayout}>
								<div className={styles.formColumn}>
									{!isPerGuildProfile && (
										<UsernameSection isClaimed={isClaimed} hasPremium={hasPremium} discriminator={user.discriminator} />
									)}

									{isPerGuildProfile && (
										<div>
											<Input
												{...form.register('nick')}
												label={t`Community Nickname`}
												placeholder={user.username}
												maxLength={32}
												value={form.watch('nick') || ''}
												footer={
													<div className={styles.inputFooter}>
														{t`This nickname will only be visible in this community`}
													</div>
												}
											/>
										</div>
									)}

									{!isPerGuildProfile && (
										<div>
											<Input
												{...form.register('global_name')}
												label={t`Display Name`}
												placeholder={user.username}
												maxLength={32}
												value={form.watch('global_name') || ''}
												error={form.formState.errors.global_name?.message}
											/>
										</div>
									)}

									<div>
										<Input
											{...form.register('pronouns')}
											label={t`Pronouns`}
											placeholder={isPerGuildProfile && user?.pronouns ? user.pronouns : t`they/them`}
											maxLength={40}
											value={form.watch('pronouns') || ''}
											error={form.formState.errors.pronouns?.message}
										/>
									</div>

									{isPerGuildProfile && !hasPremium && <PerGuildPremiumUpsell />}

									<div>
										<AvatarUploader
											hasAvatar={hasAvatar && !hasClearedAvatar}
											onAvatarChange={handleAvatarChange}
											onAvatarClear={handleAvatarClear}
											disabled={isPerGuildProfile && !hasPremium}
											hasPremium={hasPremium}
											isPerGuildProfile={isPerGuildProfile}
											errorMessage={form.formState.errors.avatar?.message}
											avatarMode={avatarMode}
											onAvatarModeChange={handleAvatarModeChange}
										/>
									</div>

									<div>
										<BannerUploader
											hasBanner={hasBanner && !hasClearedBanner}
											onBannerChange={handleBannerChange}
											onBannerClear={handleBannerClear}
											disabled={isPerGuildProfile && !hasPremium}
											hasPremium={hasPremium}
											isPerGuildProfile={isPerGuildProfile}
											errorMessage={form.formState.errors.banner?.message}
											bannerMode={bannerMode}
											onBannerModeChange={handleBannerModeChange}
										/>
									</div>

									<div className={isPerGuildProfile && !hasPremium ? styles.opacityHalf : ''}>
										<AccentColorPicker
											value={form.watch('accent_color') ?? 0}
											onChange={(value: number) =>
												form.setValue('accent_color', value === 0 ? null : value, {shouldDirty: true})
											}
											disabled={isPerGuildProfile && !hasPremium}
											errorMessage={form.formState.errors.accent_color?.message}
										/>
									</div>

									<div className={isPerGuildProfile && !hasPremium ? styles.opacityHalf : ''}>
										<BioEditor
											value={bioValue}
											onChange={(newValue: string) => {
												handleTextChange(newValue, previousValueRef.current);
												setBioValue(newValue);
											}}
											onEmojiSelect={handleBioEmojiSelect}
											placeholder={
												isPerGuildProfile && user?.bio
													? convertMarkdownToSegments(user.bio, selectedGuildId).displayText
													: t`Doc, I'm from the future. I came here in a time machine that you invented. Now, I need your help to get back to the year 1985.`
											}
											maxLength={user.maxBioLength}
											disabled={isPerGuildProfile && !hasPremium}
											hasPremium={hasPremium}
											isPerGuildProfile={isPerGuildProfile}
											isMobile={mobileLayout.enabled}
											errorMessage={form.formState.errors.bio?.message}
											textareaRef={bioTextareaRef}
											emojiPickerOpen={bioExpressionPickerOpen}
											onEmojiPickerOpenChange={setBioExpressionPickerOpen}
											containerRef={bioContainerRef}
											autocompleteQuery={autocompleteQuery}
											autocompleteOptions={autocompleteOptions}
											autocompleteType={autocompleteType}
											selectedIndex={selectedIndex}
											isAutocompleteAttached={isAutocompleteAttached}
											setSelectedIndex={setSelectedIndex}
											onCursorMove={onCursorMove}
											handleSelect={handleSelect}
											autocompleteZIndex={AUTOCOMPLETE_Z_INDEX}
										/>
									</div>
								</div>

								<div className={styles.previewColumn}>
									<ProfilePreview
										user={user}
										previewAvatarUrl={previewAvatarUrl}
										previewBannerUrl={previewBannerUrl}
										hasClearedAvatar={hasClearedAvatar}
										hasClearedBanner={hasClearedBanner}
										previewBio={displayToActual(bioValue)}
										previewPronouns={form.watch('pronouns')}
										previewAccentColor={convertNumberToHexColor(form.watch('accent_color'))}
										previewGlobalName={!isPerGuildProfile ? form.watch('global_name') : undefined}
										previewNick={isPerGuildProfile ? form.watch('nick') : undefined}
										guildId={selectedGuildId}
										guildMember={isPerGuildProfile ? guildMember : undefined}
										guildMemberProfile={isPerGuildProfile ? profileData?.guildMemberProfile : undefined}
										previewBadgeSettings={{
											premium_badge_hidden: form.watch('premium_badge_hidden'),
											premium_badge_timestamp_hidden: form.watch('premium_badge_timestamp_hidden'),
											premium_badge_masked: form.watch('premium_badge_masked'),
											premium_badge_sequence_hidden: form.watch('premium_badge_sequence_hidden'),
										}}
										ignoreGuildAvatarInPreview={isPerGuildProfile && avatarMode === 'inherit'}
										ignoreGuildBannerInPreview={isPerGuildProfile && bannerMode === 'inherit'}
									/>
								</div>
							</div>
						)}
					</SettingsTabSection>

					{hasPremium && !isPerGuildProfile && (
						<PremiumBadgeSettings
							premiumBadgeHidden={form.watch('premium_badge_hidden') ?? false}
							premiumBadgeTimestampHidden={form.watch('premium_badge_timestamp_hidden') ?? false}
							premiumBadgeMasked={form.watch('premium_badge_masked') ?? false}
							premiumBadgeSequenceHidden={form.watch('premium_badge_sequence_hidden') ?? false}
							onToggle={handlePremiumBadgeToggle}
							hasLifetimePremium={hasLifetimePremium}
							premiumSince={user.premiumSince}
							premiumLifetimeSequence={user.premiumLifetimeSequence}
						/>
					)}
				</Form>
			</SettingsTabContainer>

			{mobileLayout.enabled && (
				<ExpressionPickerSheet
					isOpen={bioExpressionPickerOpen}
					onClose={() => setBioExpressionPickerOpen(false)}
					onEmojiSelect={handleBioEmojiSelect}
					visibleTabs={['emojis']}
					selectedTab="emojis"
					zIndex={30000}
				/>
			)}
		</>
	);
});

export default MyProfileTabComponent;
