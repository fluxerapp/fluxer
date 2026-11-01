// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {MAX_TRUSTED_SITES, parseTrustedSite} from '@app/features/theme_settings/state/TrustedSites';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {TRUSTED_SITE_WARNING_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const TRUSTED_SITES_DESCRIPTOR = msg({
	message: 'Trusted sites',
	comment: 'Heading in the "Quick CSS" section of Settings > Themes in the desktop app.',
});
const TRUSTED_SITES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your quick CSS and themes can load images from these sites.',
	comment: 'Shown under the "Trusted sites" heading in Settings > Themes in the desktop app.',
});
const SITE_ADDRESS_DESCRIPTOR = msg({
	message: 'Site address',
	comment:
		'Label of the field where you type a web address like https://example.com under "Trusted sites" in Settings > Themes.',
});
const TRUST_DESCRIPTOR = msg({
	message: 'Trust',
	comment:
		'Button next to the "Site address" field under "Trusted sites" in Settings > Themes. Adds the site to the list.',
});
const INVALID_SITE_DESCRIPTOR = msg({
	message: 'Enter a site address like https://example.com.',
	comment: 'Error under the "Site address" field in Settings > Themes when the text is not a valid https web address.',
});
const TOO_MANY_SITES_DESCRIPTOR = msg({
	message: 'You can trust up to {count} sites.',
	comment: 'Error under the "Site address" field in Settings > Themes when the list of trusted sites is full.',
});
const STOP_TRUSTING_DESCRIPTOR = msg({
	message: 'Stop trusting {site}',
	comment:
		'Accessible label of the delete button on a row under "Trusted sites" in Settings > Themes. {site} is a web address.',
});

export const TrustedSitesSettings: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [value, setValue] = useState('');
	const [error, setError] = useState<string | null>(null);
	const sites = ThemeSettingsState.trustedSites;
	const handleTrust = () => {
		const site = parseTrustedSite(value);
		if (site === null) {
			setError(i18n._(INVALID_SITE_DESCRIPTOR));
			return;
		}
		if (!sites.includes(site) && sites.length >= MAX_TRUSTED_SITES) {
			setError(i18n._(TOO_MANY_SITES_DESCRIPTOR, {count: MAX_TRUSTED_SITES}));
			return;
		}
		ThemeSettingsState.trustSite(site);
		setValue('');
		setError(null);
	};
	return (
		<div className={styles.stack} data-flx="theme-settings.trusted-sites-settings.stack">
			<h4 className={styles.subsectionTitle} data-flx="theme-settings.trusted-sites-settings.subsection-title">
				{i18n._(TRUSTED_SITES_DESCRIPTOR)}
			</h4>
			<p className={styles.note} data-flx="theme-settings.trusted-sites-settings.note">
				{i18n._(TRUSTED_SITES_DESCRIPTION_DESCRIPTOR)}
			</p>
			<p className={styles.note} data-flx="theme-settings.trusted-sites-settings.warning">
				{i18n._(TRUSTED_SITE_WARNING_DESCRIPTOR)}
			</p>
			<form
				className={styles.trustedSiteForm}
				onSubmit={(event) => {
					event.preventDefault();
					handleTrust();
				}}
				data-flx="theme-settings.trusted-sites-settings.form"
			>
				<Input
					label={i18n._(SITE_ADDRESS_DESCRIPTOR)}
					value={value}
					placeholder="https://example.com"
					inputMode="url"
					autoComplete="off"
					spellCheck={false}
					error={error ?? undefined}
					onChange={(event) => {
						setValue(event.target.value);
						setError(null);
					}}
					data-flx="theme-settings.trusted-sites-settings.input.site-address"
				/>
				<Button
					type="submit"
					variant="secondary"
					fitContent
					disabled={value.trim() === ''}
					data-flx="theme-settings.trusted-sites-settings.button.trust"
				>
					{i18n._(TRUST_DESCRIPTOR)}
				</Button>
			</form>
			{sites.length > 0 ? (
				<div className={styles.list} data-flx="theme-settings.trusted-sites-settings.list">
					{sites.map((site) => (
						<div key={site} className={styles.row} data-flx="theme-settings.trusted-sites-settings.row">
							<div className={styles.rowText} data-flx="theme-settings.trusted-sites-settings.row-text">
								<span className={styles.rowLabel} data-flx="theme-settings.trusted-sites-settings.row-label">
									{site}
								</span>
							</div>
							<div className={styles.rowControls} data-flx="theme-settings.trusted-sites-settings.row-controls">
								<FocusRing offset={-2} data-flx="theme-settings.trusted-sites-settings.focus-ring">
									<button
										type="button"
										className={clsx(styles.iconButton, styles.iconButtonDanger)}
										aria-label={i18n._(STOP_TRUSTING_DESCRIPTOR, {site})}
										onClick={() => ThemeSettingsState.untrustSite(site)}
										data-flx="theme-settings.trusted-sites-settings.icon-button.stop-trusting"
									>
										<TrashIcon size={remFromPx(18)} data-flx="theme-settings.trusted-sites-settings.trash-icon" />
									</button>
								</FocusRing>
							</div>
						</div>
					))}
				</div>
			) : null}
		</div>
	);
});
