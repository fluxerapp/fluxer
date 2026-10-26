// SPDX-License-Identifier: AGPL-3.0-or-later

import {useSettingsCurrentTab} from '@app/features/app/components/dialogs/shared/SettingsCurrentTabContext';
import {SettingsHeadingLinkButton} from '@app/features/app/components/dialogs/shared/SettingsHeadingLinkButton';
import styles from '@app/features/app/components/dialogs/shared/SettingsSection.module.css';
import {Accordion} from '@app/features/ui/accordion/Accordion';
import {
	isSectionIdValid,
	tabHasMultipleLinkableSections,
	type UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {
	buildUserSettingsDeepLink,
	getUserSettingsSectionDeepLinkTarget,
} from '@app/features/user/components/settings_utils/UserSettingsDeepLinks';
import {clsx} from 'clsx';
import type React from 'react';
import {createContext, useContext, useEffect} from 'react';

export const SettingsPageContext = createContext(true);

export interface SettingsSectionProps {
	id: string;
	tabType?: UserSettingsTabType;
	title: React.ReactNode;
	description?: React.ReactNode;
	actions?: React.ReactNode;
	isAdvanced?: boolean;
	linkable?: boolean;
	defaultExpanded?: boolean;
	children: React.ReactNode;
	className?: string;
}

export const SettingsSection: React.FC<SettingsSectionProps> = ({
	id,
	tabType,
	title,
	description,
	actions,
	isAdvanced = false,
	linkable = true,
	defaultExpanded = true,
	children,
	className,
}) => {
	const contextTabType = useSettingsCurrentTab();
	const linksShown = useContext(SettingsPageContext);
	const resolvedTabType = tabType ?? contextTabType ?? undefined;
	useEffect(() => {
		if (import.meta.env.DEV && linkable && !isSectionIdValid(id, resolvedTabType)) {
			console.warn(
				`[SettingsSection] Unknown section ID "${id}" - ensure it's registered in SettingsSectionRegistry.tsx`,
			);
		}
	}, [id, linkable, resolvedTabType]);
	const linkTarget = linkable && linksShown ? getUserSettingsSectionDeepLinkTarget(id, resolvedTabType) : null;
	const linkHref =
		linkTarget && tabHasMultipleLinkableSections(linkTarget.tab)
			? buildUserSettingsDeepLink(linkTarget.tab, linkTarget.section)
			: null;
	const linkButton = linkHref ? (
		<SettingsHeadingLinkButton href={linkHref} data-flx="app.settings-section.heading-link-button" />
	) : null;
	if (isAdvanced) {
		return (
			<Accordion
				id={id}
				title={title}
				description={description}
				defaultExpanded={defaultExpanded}
				className={className}
				headerAction={linkButton}
				data-flx="app.settings-section.accordion"
			>
				{children}
			</Accordion>
		);
	}
	return (
		<section id={id} className={clsx(styles.section, className)} data-flx="app.settings-section.section">
			<div
				className={clsx(styles.sectionHeader, actions && styles.sectionHeaderWithActions)}
				data-flx="app.settings-section.section-header"
			>
				<div className={styles.sectionHeaderText} data-flx="app.settings-section.section-header-text">
					<div className={styles.sectionTitleRow} data-flx="app.settings-section.section-title-row">
						<h3 className={styles.sectionTitle} data-flx="app.settings-section.section-title">
							{title}
						</h3>
						{linkButton}
					</div>
					{description ? (
						<p className={styles.sectionDescription} data-flx="app.settings-section.section-description">
							{description}
						</p>
					) : null}
				</div>
				{actions ? (
					<div className={styles.sectionActions} data-flx="app.settings-section.section-actions">
						{actions}
					</div>
				) : null}
			</div>
			<div className={styles.sectionContent} data-flx="app.settings-section.section-content">
				{children}
			</div>
		</section>
	);
};
