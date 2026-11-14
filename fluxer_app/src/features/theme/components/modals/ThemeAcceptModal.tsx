// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {useArboriumHighlightedHtml} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {COPY_CODE_DESCRIPTOR, SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import styles from '@app/features/theme/components/modals/ThemeAcceptModal.module.css';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import {gateThemeCss} from '@app/features/theme/utils/ThemeCssUrlGate';
import {buildThemeCssFetchUrl} from '@app/features/theme/utils/ThemeUtils';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {useCurrentAccountLabels} from '@app/features/theme_settings/hooks/useCurrentAccountLabels';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {canTrustSites} from '@app/features/theme_settings/state/TrustedSites';
import {TRUSTED_SITE_WARNING_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {CopyButton} from '@app/features/ui/components/CopyButton';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';

const WE_COULDN_T_READ_THIS_THEME_IT_MAY_DESCRIPTOR = msg({
	message: "We couldn't read this theme. It may be corrupted or invalid.",
	comment: 'Error message in the theme accept modal.',
});
const THIS_THEME_IS_STILL_LOADING_DESCRIPTOR = msg({
	message: 'This theme is still loading.',
	comment: 'Description text in the theme accept modal.',
});
const THEME_APPLIED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Theme applied successfully.',
	comment: 'Short label in the theme accept modal. Keep it concise.',
});
const WE_COULDN_T_APPLY_THIS_THEME_DESCRIPTOR = msg({
	message: "We couldn't apply this theme.",
	comment: 'Error message in the theme accept modal.',
});
const REPLACES_DESKTOP_QUICK_CSS_DESCRIPTOR = msg({
	message: 'This replaces the quick CSS in the desktop app theme.',
	comment: 'Shown in the modal that applies a shared theme while the desktop app theme is being edited.',
});
const REPLACES_ACCOUNT_QUICK_CSS_DESCRIPTOR = msg({
	message: 'This replaces the quick CSS for {username} on {instanceName}.',
	comment:
		"Shown in the modal that applies a shared theme to the current account's own theme. {username} is the account's username and {instanceName} is its instance.",
});
const OPEN_IN_THEMES_DESCRIPTOR = msg({
	message: 'Open in Themes',
	comment: 'Button in the modal that applied a shared theme. Opens Settings > Themes.',
});
const BLOCKED_FILES_DESCRIPTOR = msg({
	message: "Fluxer won't load {count, plural, one {# file} other {# files}} that this theme links to on other sites.",
	comment: 'Shown in the modal that applies a shared theme when the theme points at files the app will not load.',
});
const UNTRUSTED_SITES_DESCRIPTOR = msg({
	message: "This theme loads images from sites you haven't trusted. Check the sites you trust to load their images.",
	comment:
		'Shown in the modal that applies a shared theme in the desktop app, above a list of web addresses with a checkbox each.',
});
const I_TRUST_SITE_DESCRIPTOR = msg({
	message: 'I trust {site}',
	comment:
		'Checkbox label in the modal that applies a shared theme. {site} is a web address like https://example.com. When checked, the site is trusted when the theme is applied.',
});
const logger = new Logger('ThemeAcceptModal');
const NO_OBJECT_URLS: ReadonlySet<string> = new Set();
const NO_SITES: ReadonlyArray<string> = [];

interface ThemeAcceptModalProps {
	themeId: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}

export const ThemeAcceptModal = observer(function ThemeAcceptModal({themeId, runtimeSnapshot}: ThemeAcceptModalProps) {
	const {i18n} = useLingui();
	const {username, instanceName} = useCurrentAccountLabels();
	const target = ThemeEditTarget.target;
	const [isApplying, setIsApplying] = useState(false);
	const [isApplied, setIsApplied] = useState(false);
	const [css, setCss] = useState<string | null>(null);
	const [fetchStatus, setFetchStatus] = useState<'idle' | 'loading' | 'error' | 'ready'>('idle');
	const [fetchError, setFetchError] = useState<string | null>(null);
	const highlightedCss = useArboriumHighlightedHtml('css', css);
	const trustedSites = canTrustSites() ? ThemeSettingsState.trustedSites : NO_SITES;
	const [sitesToTrust, setSitesToTrust] = useState<ReadonlySet<string>>(new Set());
	const gated = useMemo(() => {
		if (!css) return null;
		return gateThemeCss(css, {
			mediaEndpoint: runtimeSnapshot.mediaEndpoint,
			objectUrls: NO_OBJECT_URLS,
			trustedSites: new Set(trustedSites),
		});
	}, [css, runtimeSnapshot, trustedSites]);
	const blockedCount = gated?.blocked.length ?? 0;
	const untrustedSites = canTrustSites() ? (gated?.blockedSites ?? NO_SITES) : NO_SITES;
	const setSiteTrusted = (site: string, trusted: boolean) => {
		setSitesToTrust((current) => {
			const next = new Set(current);
			if (trusted) next.add(site);
			else next.delete(site);
			return next;
		});
	};
	useEffect(() => {
		let cancelled = false;
		const fetchTheme = async () => {
			setFetchStatus('loading');
			setFetchError(null);
			try {
				const themeUrl = buildThemeCssFetchUrl(runtimeSnapshot, themeId);
				if (!themeUrl) {
					throw new Error('Media endpoint not configured');
				}
				const response = await fetch(themeUrl);
				if (!response.ok) {
					throw new Error('Theme not found');
				}
				const text = await response.text();
				if (cancelled) return;
				setCss(text);
				setFetchStatus('ready');
			} catch (error) {
				if (cancelled) return;
				logger.error('Failed to fetch theme:', error);
				setCss(null);
				setFetchStatus('error');
				setFetchError(i18n._(WE_COULDN_T_READ_THIS_THEME_IT_MAY_DESCRIPTOR));
			}
		};
		void fetchTheme();
		return () => {
			cancelled = true;
		};
	}, [runtimeSnapshot, themeId]);
	const handleDismiss = () => {
		ModalCommands.pop();
	};
	const handleApply = async () => {
		if (!css) {
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => fetchError ?? i18n._(THIS_THEME_IS_STILL_LOADING_DESCRIPTOR),
				dataFlx: 'theme.theme-accept-modal.theme-not-ready-error-modal',
			});
			return;
		}
		setIsApplying(true);
		try {
			for (const site of untrustedSites) {
				if (sitesToTrust.has(site)) ThemeSettingsState.trustSite(site);
			}
			ThemeSettingsCommands.setEditedCustomThemeCss(css);
			ToastCommands.success(i18n._(THEME_APPLIED_SUCCESSFULLY_DESCRIPTOR));
			setIsApplied(true);
			setIsApplying(false);
		} catch (error) {
			logger.error('Failed to apply theme:', error);
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => i18n._(WE_COULDN_T_APPLY_THIS_THEME_DESCRIPTOR),
				dataFlx: 'theme.theme-accept-modal.apply-theme-error-modal',
			});
			setIsApplying(false);
		}
	};
	const renderCodeContent = () => {
		if (fetchStatus === 'loading') {
			return (
				<span className={styles.loadingText} data-flx="theme.theme-accept-modal.render-code-content.loading-text">
					<Trans>Loading theme...</Trans>
				</span>
			);
		}
		if (fetchStatus === 'error') {
			return (
				<span className={styles.errorText} data-flx="theme.theme-accept-modal.render-code-content.error-text">
					{fetchError}
				</span>
			);
		}
		if (!css) {
			return null;
		}
		return (
			<code
				className={styles.hljs}
				dangerouslySetInnerHTML={{__html: highlightedCss}}
				data-flx="theme.theme-accept-modal.render-code-content.hljs"
			/>
		);
	};
	return (
		<Modal.Root size="medium" data-flx="theme.theme-accept-modal.modal-root">
			<Modal.Header title={<Trans>Import theme</Trans>} data-flx="theme.theme-accept-modal.modal-header" />
			<Modal.Content padding="none" className={styles.content} data-flx="theme.theme-accept-modal.content">
				<p className={styles.description} data-flx="theme.theme-accept-modal.description">
					{target === 'desktop'
						? i18n._(REPLACES_DESKTOP_QUICK_CSS_DESCRIPTOR)
						: i18n._(REPLACES_ACCOUNT_QUICK_CSS_DESCRIPTOR, {username, instanceName})}
				</p>
				{blockedCount > 0 && (
					<p className={styles.description} data-flx="theme.theme-accept-modal.blocked-files">
						{i18n._(BLOCKED_FILES_DESCRIPTOR, {count: blockedCount})}
					</p>
				)}
				{untrustedSites.length > 0 && !isApplied && (
					<div className={styles.trustSites} data-flx="theme.theme-accept-modal.trust-sites">
						<p className={styles.description} data-flx="theme.theme-accept-modal.untrusted-sites">
							{i18n._(UNTRUSTED_SITES_DESCRIPTOR)}
						</p>
						<p className={styles.description} data-flx="theme.theme-accept-modal.trusted-site-warning">
							{i18n._(TRUSTED_SITE_WARNING_DESCRIPTOR)}
						</p>
						{untrustedSites.map((site) => (
							<Checkbox
								key={site}
								checked={sitesToTrust.has(site)}
								size="small"
								onChange={(checked) => setSiteTrusted(site, checked)}
								data-flx="theme.theme-accept-modal.checkbox.trust-site"
							>
								{i18n._(I_TRUST_SITE_DESCRIPTOR, {site})}
							</Checkbox>
						))}
					</div>
				)}
				<div className={styles.codeContainer} data-flx="theme.theme-accept-modal.code-container">
					<CopyButton
						value={css ?? ''}
						label={COPY_CODE_DESCRIPTOR}
						className={styles.codeActions}
						visibleClassName={styles.codeActionsVisible}
						buttonClassName={styles.copyButton}
						iconClassName={styles.copyIcon}
						disabled={!css}
						data-flx="theme.theme-accept-modal.code-actions"
					/>
					<pre className={styles.pre} data-flx="theme.theme-accept-modal.pre">
						{renderCodeContent()}
					</pre>
				</div>
			</Modal.Content>
			<Modal.Footer className={styles.footer} data-flx="theme.theme-accept-modal.footer">
				{isApplied ? (
					<>
						<Button variant="secondary" onClick={handleDismiss} data-flx="theme.theme-accept-modal.button.done">
							<Trans>Done</Trans>
						</Button>
						<Button
							onClick={() => {
								ModalCommands.pop();
								ThemeSettingsCommands.openThemesSettings();
							}}
							data-flx="theme.theme-accept-modal.button.open-in-themes"
						>
							{i18n._(OPEN_IN_THEMES_DESCRIPTOR)}
						</Button>
					</>
				) : (
					<>
						<Button
							variant="secondary"
							onClick={handleDismiss}
							disabled={isApplying}
							data-flx="theme.theme-accept-modal.button.dismiss"
						>
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handleApply}
							disabled={isApplying || fetchStatus !== 'ready'}
							submitting={isApplying}
							data-flx="theme.theme-accept-modal.button.apply"
						>
							<Trans>Apply</Trans>
						</Button>
					</>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
