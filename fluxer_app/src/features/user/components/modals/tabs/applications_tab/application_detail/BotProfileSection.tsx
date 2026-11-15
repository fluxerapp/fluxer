// SPDX-License-Identifier: AGPL-3.0-or-later

import {ImagePreviewField} from '@app/features/app/components/shared/ImagePreviewField';
import {EXAMPLE_BOT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {DeveloperApplication} from '@app/features/devtools/models/DeveloperApplication';
import {USERNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {UsernameValidationRules} from '@app/features/ui/components/form/UsernameValidationRules';
import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import {SectionCard} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailSectionCard';
import type {ApplicationDetailForm} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailTypes';
import {AvatarUploader} from '@app/features/user/components/modals/tabs/my_profile_tab/AvatarUploader';
import {BannerUploader} from '@app/features/user/components/modals/tabs/my_profile_tab/BannerUploader';
import {shouldShowFriendSettings} from '@app/features/user/components/settings_utils/SettingsVisibility';
import {
	BOT_AVATAR_DESCRIPTOR,
	BOT_PROFILE_DESCRIPTOR,
	BOT_USERNAME_DESCRIPTOR,
	FRIENDLY_BOT_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {Controller} from 'react-hook-form';

const AVATAR_TAG_AND_RICH_PROFILE_DETAILS_FOR_YOUR_DESCRIPTOR = msg({
	message: 'Avatar, tag, and rich profile details for your bot.',
	comment: 'Description text in the bot profile section.',
});

const USERNAME_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Username is required',
	comment: 'Short label in the bot profile section. Keep it concise.',
});
const USERNAME_MUST_BE_AT_LEAST_1_CHARACTER_DESCRIPTOR = msg({
	message: 'Username must be at least 1 character',
	comment: 'Label in the bot profile section.',
});
const USERNAME_MUST_BE_AT_MOST_32_CHARACTERS_DESCRIPTOR = msg({
	message: 'Username must be at most 32 characters',
	comment: 'Label in the bot profile section.',
});
const USERNAME_CAN_ONLY_CONTAIN_LETTERS_NUMBERS_AND_UNDERSCORES_DESCRIPTOR = msg({
	message: 'Username can only contain letters, numbers, and underscores',
	comment: 'Label in the bot profile section.',
});

const DISCRIMINATOR_DESCRIPTOR = msg({
	message: 'Discriminator',
	comment: 'Short label in the bot profile section. Keep it concise.',
});
const BOT_BIO_DESCRIPTOR = msg({
	message: 'Bot bio',
	comment: 'Short label in the bot profile section. Keep it concise.',
});
const A_HELPFUL_BOT_THAT_DOES_AMAZING_THINGS_DESCRIPTOR = msg({
	message: 'A helpful bot that does amazing things!',
	comment: 'Description text in the bot profile section.',
});
const NO_BOT_BANNER_DESCRIPTOR = msg({
	message: 'No bot banner',
	comment: 'Empty-state text in the bot profile section. Keep the tone plain and specific.',
});
const BOT_BANNER_PREVIEW_DESCRIPTOR = msg({
	message: 'Bot banner preview',
	comment: 'Short label in the bot profile section. Keep it concise. Keep the tone plain and specific.',
});

const ALLOW_USERS_TO_SEND_THIS_BOT_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Allow users to send this bot friend requests for manual approval.',
	comment: 'Description text in the bot profile section.',
});
const REQUIRE_MANUAL_FRIEND_APPROVAL_DESCRIPTOR = msg({
	message: 'Require manual friend approval',
	comment: 'Label in the bot profile section.',
});
const WHEN_ENABLED_YOU_MUST_ACCEPT_FRIEND_REQUESTS_TO_DESCRIPTOR = msg({
	message: 'Friend requests to this bot need manual approval.',
	comment: 'Description text in the bot profile section.',
});

interface BotProfileSectionProps {
	application: DeveloperApplication;
	form: ApplicationDetailForm;
	displayAvatarUrl: string | null;
	hasAvatar: boolean;
	hasClearedAvatar: boolean;
	displayBannerUrl: string | null;
	hasBanner: boolean;
	hasClearedBanner: boolean;
	onAvatarChange: (value: string) => void;
	onAvatarClear: () => void;
	onBannerChange: (value: string) => void;
	onBannerClear: () => void;
}

export const BotProfileSection: React.FC<BotProfileSectionProps> = ({
	application,
	form,
	displayAvatarUrl,
	hasAvatar,
	hasClearedAvatar,
	displayBannerUrl,
	hasBanner,
	hasClearedBanner,
	onAvatarChange,
	onAvatarClear,
	onBannerChange,
	onBannerClear,
}) => {
	const {i18n} = useLingui();
	const friendlyFlagEnabled = form.watch('friendlyBot') ?? false;
	return (
		<SectionCard
			title={i18n._(BOT_PROFILE_DESCRIPTOR)}
			subtitle={i18n._(AVATAR_TAG_AND_RICH_PROFILE_DETAILS_FOR_YOUR_DESCRIPTOR)}
			data-flx="user.applications-tab.application-detail.bot-profile-section.section-card"
		>
			<div
				className={styles.fieldStack}
				data-flx="user.applications-tab.application-detail.bot-profile-section.field-stack"
			>
				<div
					className={styles.avatarRow}
					data-flx="user.applications-tab.application-detail.bot-profile-section.avatar-row"
				>
					{displayAvatarUrl ? (
						<img
							src={displayAvatarUrl}
							alt={i18n._(BOT_AVATAR_DESCRIPTOR)}
							className={styles.avatarPreview}
							data-flx="user.applications-tab.application-detail.bot-profile-section.avatar-preview"
						/>
					) : (
						<div
							className={styles.avatarPlaceholder}
							data-flx="user.applications-tab.application-detail.bot-profile-section.avatar-placeholder"
						>
							{application.bot?.username.charAt(0).toUpperCase()}
						</div>
					)}
					<AvatarUploader
						hasAvatar={hasAvatar && !hasClearedAvatar}
						onAvatarChange={onAvatarChange}
						onAvatarClear={onAvatarClear}
						requireAnimatedAvatarEntitlement={false}
						isPerGuildProfile={false}
						errorMessage={form.formState.errors.avatar?.message}
						data-flx="user.applications-tab.application-detail.bot-profile-section.avatar-uploader"
					/>
				</div>
				<div className={styles.tagRow} data-flx="user.applications-tab.application-detail.bot-profile-section.tag-row">
					<Controller
						name="username"
						control={form.control}
						rules={{
							required: i18n._(USERNAME_IS_REQUIRED_DESCRIPTOR),
							minLength: {value: 1, message: i18n._(USERNAME_MUST_BE_AT_LEAST_1_CHARACTER_DESCRIPTOR)},
							maxLength: {value: 32, message: i18n._(USERNAME_MUST_BE_AT_MOST_32_CHARACTERS_DESCRIPTOR)},
							pattern: {
								value: /^[a-zA-Z0-9_]+$/,
								message: i18n._(USERNAME_CAN_ONLY_CONTAIN_LETTERS_NUMBERS_AND_UNDERSCORES_DESCRIPTOR),
							},
						}}
						render={({field}) => (
							<Input
								data-flx="user.applications-tab.application-detail.bot-profile-section.input"
								{...field}
								aria-label={i18n._(BOT_USERNAME_DESCRIPTOR)}
								placeholder={EXAMPLE_BOT_NAME}
								maxLength={32}
								required
								label={i18n._(USERNAME_DESCRIPTOR)}
							/>
						)}
						data-flx="user.applications-tab.application-detail.bot-profile-section.controller"
					/>
					<div
						className={styles.discriminatorInput}
						data-flx="user.applications-tab.application-detail.bot-profile-section.discriminator-input"
					>
						<Input
							value={application.bot?.discriminator}
							readOnly
							disabled
							maxLength={4}
							aria-label={i18n._(DISCRIMINATOR_DESCRIPTOR)}
							data-flx="user.applications-tab.application-detail.bot-profile-section.input--2"
						/>
					</div>
				</div>
				{form.formState.errors.username && (
					<div className={styles.error} data-flx="user.applications-tab.application-detail.bot-profile-section.error">
						{form.formState.errors.username?.message}
					</div>
				)}
				<div
					className={styles.validationBox}
					data-flx="user.applications-tab.application-detail.bot-profile-section.validation-box"
				>
					<UsernameValidationRules
						username={form.watch('username') || ''}
						data-flx="user.applications-tab.application-detail.bot-profile-section.username-validation-rules"
					/>
				</div>
				<Controller
					name="bio"
					control={form.control}
					render={({field}) => (
						<Textarea
							ref={field.ref}
							name={field.name}
							onBlur={field.onBlur}
							label={i18n._(BOT_BIO_DESCRIPTOR)}
							value={field.value ?? ''}
							onChange={(event) => field.onChange(event.target.value)}
							placeholder={i18n._(A_HELPFUL_BOT_THAT_DOES_AMAZING_THINGS_DESCRIPTOR)}
							minRows={3}
							maxRows={6}
							maxLength={1024}
							error={form.formState.errors.bio?.message}
							data-flx="user.applications-tab.application-detail.bot-profile-section.textarea.change"
						/>
					)}
					data-flx="user.applications-tab.application-detail.bot-profile-section.controller--2"
				/>
				<div
					className={styles.bannerRow}
					data-flx="user.applications-tab.application-detail.bot-profile-section.banner-row"
				>
					<BannerUploader
						hasBanner={hasBanner}
						onBannerChange={onBannerChange}
						onBannerClear={onBannerClear}
						requireBannerEntitlement={false}
						isPerGuildProfile={false}
						errorMessage={form.formState.errors.banner?.message as string | undefined}
						data-flx="user.applications-tab.application-detail.bot-profile-section.banner-uploader"
					/>
				</div>
				<ImagePreviewField
					imageUrl={hasBanner && !hasClearedBanner ? displayBannerUrl : null}
					showPlaceholder={!hasBanner || hasClearedBanner}
					placeholderText={i18n._(NO_BOT_BANNER_DESCRIPTOR)}
					altText={i18n._(BOT_BANNER_PREVIEW_DESCRIPTOR)}
					objectFit="contain"
					data-flx="user.applications-tab.application-detail.bot-profile-section.image-preview-field"
				/>
				{shouldShowFriendSettings() && (
					<>
						<Controller
							name="friendlyBot"
							control={form.control}
							render={({field}) => (
								<Switch
									label={i18n._(FRIENDLY_BOT_DESCRIPTOR)}
									description={i18n._(ALLOW_USERS_TO_SEND_THIS_BOT_FRIEND_REQUESTS_DESCRIPTOR)}
									value={Boolean(field.value)}
									onChange={field.onChange}
									data-flx="user.applications-tab.application-detail.bot-profile-section.switch.change"
								/>
							)}
							data-flx="user.applications-tab.application-detail.bot-profile-section.controller--3"
						/>
						<Controller
							name="botManualFriendRequestApproval"
							control={form.control}
							render={({field}) => (
								<Switch
									label={i18n._(REQUIRE_MANUAL_FRIEND_APPROVAL_DESCRIPTOR)}
									description={i18n._(WHEN_ENABLED_YOU_MUST_ACCEPT_FRIEND_REQUESTS_TO_DESCRIPTOR)}
									value={Boolean(field.value)}
									onChange={field.onChange}
									disabled={!friendlyFlagEnabled}
									data-flx="user.applications-tab.application-detail.bot-profile-section.switch.change--2"
								/>
							)}
							data-flx="user.applications-tab.application-detail.bot-profile-section.controller--4"
						/>
					</>
				)}
			</div>
		</SectionCard>
	);
};
