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

import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';

import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {Form} from '~/components/form/Form';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import {GUILD_OVERVIEW_TAB_ID, useGuildOverviewData} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';

import styles from './GuildOverviewTab.module.css';
import {BrandingSection} from './sections/BrandingSection';
import {DefaultNotificationsSection} from './sections/DefaultNotificationsSection';
import {DisallowUnclaimedAccountsSection} from './sections/DisallowUnclaimedAccountsSection';
import {IdleSettingsSection} from './sections/IdleSettingsSection';
import {SystemWelcomeSection} from './sections/SystemWelcomeSection';
import {TextChannelNamesSection} from './sections/TextChannelNamesSection';

const GuildOverviewTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const data = useGuildOverviewData(guildId);

	const {
		guild,
		voiceChannels,
		textChannels,
		defaultValues,
		handleReset,
		onSubmit,

		hasClearedIcon,
		setHasClearedIcon,
		previewIconUrl,
		setPreviewIconUrl,

		hasClearedBanner,
		setHasClearedBanner,
		previewBannerUrl,
		setPreviewBannerUrl,

		hasClearedSplash,
		setHasClearedSplash,
		previewSplashUrl,
		setPreviewSplashUrl,

		hasClearedEmbedSplash,
		setHasClearedEmbedSplash,
		previewEmbedSplashUrl,
		setPreviewEmbedSplashUrl,

		bannerAspectRatio,
		setBannerAspectRatio,
		splashAspectRatio,
		setSplashAspectRatio,

		canManageGuild,
		computeAspectRatioFromBase64,
	} = data;

	const form = useForm<FormInputs>({defaultValues});

	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit: (values) => onSubmit(values, form),
		defaultErrorField: 'name',
	});

	const isFormDirty = form.formState.isDirty;
	const isSubmitting = form.formState.isSubmitting;

	// NOTE: do NOT rely on isDirty alone; clearing assets may keep the form value == defaultValue.
	const hasUnsavedChanges = Boolean(
		isFormDirty ||
			previewIconUrl ||
			hasClearedIcon ||
			previewBannerUrl ||
			hasClearedBanner ||
			previewSplashUrl ||
			hasClearedSplash ||
			previewEmbedSplashUrl ||
			hasClearedEmbedSplash,
	);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(GUILD_OVERVIEW_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(GUILD_OVERVIEW_TAB_ID, {
			onReset: () => handleReset(form),
			onSave: handleSave,
			isSubmitting,
		});
	}, [handleReset, handleSave, form, isSubmitting]);

	React.useEffect(() => {
		return () => {
			UnsavedChangesActionCreators.clearUnsavedChanges(GUILD_OVERVIEW_TAB_ID);
		};
	}, []);

	React.useEffect(() => {
		if (!guild) return;
		if (hasUnsavedChanges) return;
		handleReset(form);
	}, [guild?.id, defaultValues, handleReset, form, hasUnsavedChanges]);

	if (!guild) return null;

	return (
		<div className={styles.container}>
			<Form form={form} onSubmit={handleSave}>
				<BrandingSection
					guildId={guildId}
					guild={guild as any}
					form={form}
					canManageGuild={canManageGuild}
					previewIconUrl={previewIconUrl}
					setPreviewIconUrl={setPreviewIconUrl}
					hasClearedIcon={hasClearedIcon}
					setHasClearedIcon={setHasClearedIcon}
					previewBannerUrl={previewBannerUrl}
					setPreviewBannerUrl={setPreviewBannerUrl}
					hasClearedBanner={hasClearedBanner}
					setHasClearedBanner={setHasClearedBanner}
					bannerAspectRatio={bannerAspectRatio}
					setBannerAspectRatio={setBannerAspectRatio}
					previewSplashUrl={previewSplashUrl}
					setPreviewSplashUrl={setPreviewSplashUrl}
					hasClearedSplash={hasClearedSplash}
					setHasClearedSplash={setHasClearedSplash}
					splashAspectRatio={splashAspectRatio}
					setSplashAspectRatio={setSplashAspectRatio}
					previewEmbedSplashUrl={previewEmbedSplashUrl}
					setPreviewEmbedSplashUrl={setPreviewEmbedSplashUrl}
					hasClearedEmbedSplash={hasClearedEmbedSplash}
					setHasClearedEmbedSplash={setHasClearedEmbedSplash}
					computeAspectRatioFromBase64={computeAspectRatioFromBase64}
				/>

				<IdleSettingsSection form={form} canManageGuild={canManageGuild} voiceChannels={voiceChannels as any} />

				<SystemWelcomeSection form={form} canManageGuild={canManageGuild} textChannels={textChannels as any} />

				<DefaultNotificationsSection form={form} canManageGuild={canManageGuild} guildId={guildId} />

				<TextChannelNamesSection form={form} canManageGuild={canManageGuild} />

				<DisallowUnclaimedAccountsSection form={form} canManageGuild={canManageGuild} />
			</Form>
		</div>
	);
});

export default GuildOverviewTab;
