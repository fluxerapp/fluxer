// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTab.module.css';
import type {
	ChannelLike,
	GuildLike,
} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTypes';
import {BrandingSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/BrandingSection';
import {DefaultNotificationsSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/DefaultNotificationsSection';
import {GuildVisitsSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/GuildVisitsSection';
import {IdleSettingsSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/IdleSettingsSection';
import {OwnerCrownSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/OwnerCrownSection';
import {SystemWelcomeSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/SystemWelcomeSection';
import {TextChannelNamesSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/TextChannelNamesSection';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {GUILD_OVERVIEW_TAB_ID, useGuildOverviewData} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';
import {useForm} from 'react-hook-form';

const ADVANCED_DESCRIPTOR = msg({
	message: 'Advanced',
	comment: 'Collapsed-by-default group at the end of the community overview tab holding less common settings.',
});
const GuildOverviewTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const data = useGuildOverviewData(guildId);
	const {
		guild,
		voiceChannels,
		textChannels,
		defaultValues,
		clearLocalAssetState,
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
	useRemoteFormReset<FormInputs>({
		form,
		identityKey: guildId,
		remoteValues: guild ? defaultValues : null,
		isDirty: hasUnsavedChanges,
		onApply: clearLocalAssetState,
	});
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(GUILD_OVERVIEW_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(GUILD_OVERVIEW_TAB_ID, {
			onReset: () => handleReset(form),
			onSave: handleSave,
			isSubmitting,
		});
	}, [handleReset, handleSave, form, isSubmitting]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(GUILD_OVERVIEW_TAB_ID);
		};
	}, []);
	if (!guild) return null;
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-overview-tab.container">
			<Form form={form} onSubmit={handleSave} data-flx="guild.guild-tabs.guild-overview-tab.form.save">
				<BrandingSection
					guildId={guildId}
					guild={guild as GuildLike}
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
					data-flx="guild.guild-tabs.guild-overview-tab.branding-section"
				/>
				<IdleSettingsSection
					form={form}
					canManageGuild={canManageGuild}
					voiceChannels={voiceChannels as Array<ChannelLike>}
					data-flx="guild.guild-tabs.guild-overview-tab.idle-settings-section"
				/>
				<SystemWelcomeSection
					form={form}
					canManageGuild={canManageGuild}
					textChannels={textChannels as Array<ChannelLike>}
					data-flx="guild.guild-tabs.guild-overview-tab.system-welcome-section"
				/>
				<GuildVisitsSection
					guildId={guildId}
					canManageGuild={canManageGuild}
					data-flx="guild.guild-tabs.guild-overview-tab.guild-visits-section"
				/>
				<DefaultNotificationsSection
					form={form}
					canManageGuild={canManageGuild}
					guildId={guildId}
					data-flx="guild.guild-tabs.guild-overview-tab.default-notifications-section"
				/>
				<SettingsSection
					id="guild-overview-advanced"
					title={i18n._(ADVANCED_DESCRIPTOR)}
					isAdvanced
					linkable={false}
					defaultExpanded={false}
					data-flx="guild.guild-tabs.guild-overview-tab.guild-overview-advanced"
				>
					<TextChannelNamesSection
						form={form}
						canManageGuild={canManageGuild}
						data-flx="guild.guild-tabs.guild-overview-tab.text-channel-names-section"
					/>
					<OwnerCrownSection
						form={form}
						canManageGuild={canManageGuild}
						data-flx="guild.guild-tabs.guild-overview-tab.owner-crown-section"
					/>
				</SettingsSection>
			</Form>
		</div>
	);
});

export default GuildOverviewTab;
