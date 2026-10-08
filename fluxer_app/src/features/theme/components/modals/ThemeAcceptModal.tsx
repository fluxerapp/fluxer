// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {useArboriumHighlightedHtml} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {COPY_CODE_DESCRIPTOR, SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import styles from '@app/features/theme/components/modals/ThemeAcceptModal.module.css';
import {buildThemeCssFetchUrl} from '@app/features/theme/utils/ThemeUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {CopyButton} from '@app/features/ui/components/CopyButton';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

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
const logger = new Logger('ThemeAcceptModal');

interface ThemeAcceptModalProps {
	themeId: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}

export const ThemeAcceptModal = observer(function ThemeAcceptModal({themeId, runtimeSnapshot}: ThemeAcceptModalProps) {
	const {i18n} = useLingui();
	const themeSettingsPath = formatUserSettingsPath(i18n, 'appearance', 'theme');
	const [isApplying, setIsApplying] = useState(false);
	const [css, setCss] = useState<string | null>(null);
	const [fetchStatus, setFetchStatus] = useState<'idle' | 'loading' | 'error' | 'ready'>('idle');
	const [fetchError, setFetchError] = useState<string | null>(null);
	const highlightedCss = useArboriumHighlightedHtml('css', css);
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
			AccessibilityCommands.update({customThemeCss: css});
			ToastCommands.success(i18n._(THEME_APPLIED_SUCCESSFULLY_DESCRIPTOR));
			ModalCommands.pop();
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
					<Trans>This will replace your current custom theme. You can edit it later in {themeSettingsPath}.</Trans>
				</p>
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
			</Modal.Footer>
		</Modal.Root>
	);
});
