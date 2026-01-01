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
import type React from 'react';
import {Controller} from 'react-hook-form';
import {Input, Textarea} from '~/components/form/Input';
import {UsernameValidationRules} from '~/components/form/UsernameValidationRules';
import {AvatarUploader} from '~/components/modals/tabs/MyProfileTab/AvatarUploader';
import {BannerUploader} from '~/components/modals/tabs/MyProfileTab/BannerUploader';
import {ImagePreviewField} from '~/components/shared/ImagePreviewField';
import type {DeveloperApplication} from '~/records/DeveloperApplicationRecord';
import styles from './ApplicationDetail.module.css';
import {SectionCard} from './SectionCard';
import type {ApplicationDetailForm} from './types';

interface BotProfileSectionProps {
	application: DeveloperApplication;
	form: ApplicationDetailForm;
	displayAvatarUrl: string | null;
	hasAvatar: boolean;
	hasClearedAvatar: boolean;
	displayBannerUrl: string | null;
	hasBanner: boolean;
	hasClearedBanner: boolean;
	isLifetimePremium: boolean;
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
	isLifetimePremium,
	onAvatarChange,
	onAvatarClear,
	onBannerChange,
	onBannerClear,
}) => {
	const {t} = useLingui();
	return (
		<SectionCard title={t`Bot profile`} subtitle={t`Avatar, tag, and rich profile details for your bot.`}>
			<div className={styles.fieldStack}>
				<div className={styles.avatarRow}>
					{displayAvatarUrl ? (
						<img src={displayAvatarUrl} alt="Bot avatar" className={styles.avatarPreview} />
					) : (
						<div className={styles.avatarPlaceholder}>{application.bot?.username.charAt(0).toUpperCase()}</div>
					)}
					<AvatarUploader
						hasAvatar={hasAvatar && !hasClearedAvatar}
						onAvatarChange={onAvatarChange}
						onAvatarClear={onAvatarClear}
						hasPremium={true}
						isPerGuildProfile={false}
						errorMessage={form.formState.errors.avatar?.message}
					/>
				</div>

				<div className={styles.tagRow}>
					<Controller
						name="username"
						control={form.control}
						rules={{
							required: t`Username is required`,
							minLength: {value: 1, message: t`Username must be at least 1 character`},
							maxLength: {value: 32, message: t`Username must be at most 32 characters`},
							pattern: {
								value: /^[a-zA-Z0-9_]+$/,
								message: t`Username can only contain letters, numbers, and underscores`,
							},
						}}
						render={({field}) => (
							<Input
								{...field}
								aria-label={t`Bot Username`}
								placeholder={t`BotName`}
								maxLength={32}
								required
								label={t`FluxerTag`}
							/>
						)}
					/>
					<div className={styles.discriminatorInput}>
						{isLifetimePremium ? (
							<Controller
								name="discriminator"
								control={form.control}
								rules={{
									pattern: {
										value: /^\d{1,4}$/,
										message: t`Discriminator must be 1-4 digits`,
									},
									validate: (value) => {
										if (!value) return true;
										const num = parseInt(value, 10);
										if (num < 0 || num > 9999) {
											return t`Discriminator must be between 0 and 9999`;
										}
										return true;
									},
								}}
								render={({field}) => (
									<Input {...field} aria-label={t`Discriminator`} placeholder="0000" maxLength={4} />
								)}
							/>
						) : (
							<Input
								value={application.bot?.discriminator}
								readOnly
								disabled
								maxLength={4}
								aria-label={t`Discriminator`}
							/>
						)}
					</div>
				</div>

				{(form.formState.errors.username || form.formState.errors.discriminator) && (
					<div className={styles.error}>
						{form.formState.errors.username?.message || form.formState.errors.discriminator?.message}
					</div>
				)}

				<div className={styles.validationBox}>
					<UsernameValidationRules username={form.watch('username') || ''} />
				</div>

				<Textarea
					{...form.register('bio')}
					label={t`Bot Bio`}
					value={form.watch('bio') || ''}
					placeholder={t`A helpful bot that does amazing things!`}
					minRows={3}
					maxRows={6}
					maxLength={1024}
					error={form.formState.errors.bio?.message}
				/>

				<div className={styles.bannerRow}>
					<BannerUploader
						hasBanner={hasBanner}
						onBannerChange={onBannerChange}
						onBannerClear={onBannerClear}
						hasPremium={true}
						isPerGuildProfile={false}
						errorMessage={form.formState.errors.banner?.message as string | undefined}
					/>
				</div>

				<ImagePreviewField
					imageUrl={hasBanner && !hasClearedBanner ? displayBannerUrl : null}
					showPlaceholder={!hasBanner || hasClearedBanner}
					placeholderText={t`No bot banner`}
					altText={t`Bot banner preview`}
					objectFit="contain"
				/>
			</div>
		</SectionCard>
	);
};
