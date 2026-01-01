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

import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, ClipboardIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import highlight from 'highlight.js';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';
import styles from './ThemeAcceptModal.module.css';

interface ThemeAcceptModalProps {
	themeId: string;
}

const buildThemeUrl = (endpoint: string, themeId: string): string => {
	const base = endpoint.replace(/\/$/, '');
	return `${base}/themes/${themeId}.css`;
};

export const ThemeAcceptModal = observer(function ThemeAcceptModal({themeId}: ThemeAcceptModalProps) {
	const {t, i18n} = useLingui();
	const [isApplying, setIsApplying] = React.useState(false);
	const [isCopied, setIsCopied] = React.useState(false);
	const [css, setCss] = React.useState<string | null>(null);
	const [fetchStatus, setFetchStatus] = React.useState<'idle' | 'loading' | 'error' | 'ready'>('idle');
	const [fetchError, setFetchError] = React.useState<string | null>(null);
	const mediaEndpoint = RuntimeConfigStore.mediaEndpoint;

	React.useEffect(() => {
		if (!mediaEndpoint) {
			setCss(null);
			setFetchStatus('idle');
			setFetchError(null);
			return;
		}

		let cancelled = false;

		const fetchTheme = async () => {
			setFetchStatus('loading');
			setFetchError(null);

			try {
				const response = await fetch(buildMediaProxyURL(buildThemeUrl(mediaEndpoint, themeId)));
				if (!response.ok) {
					throw new Error('Theme not found');
				}

				const text = await response.text();
				if (cancelled) return;

				setCss(text);
				setFetchStatus('ready');
			} catch (error) {
				if (cancelled) return;
				console.error('Failed to fetch theme:', error);
				setCss(null);
				setFetchStatus('error');
				setFetchError(t`We couldn't read this theme. It may be corrupted or invalid.`);
			}
		};

		void fetchTheme();

		return () => {
			cancelled = true;
		};
	}, [mediaEndpoint, themeId]);

	const handleDismiss = () => {
		ModalActionCreators.pop();
	};

	const handleCopy = () => {
		if (!css) return;
		TextCopyActionCreators.copy(i18n, css);
		setIsCopied(true);
		setTimeout(() => setIsCopied(false), 2000);
	};

	const handleApply = async () => {
		if (!css) {
			ToastActionCreators.error(fetchError ?? t`This theme is still loading.`);
			return;
		}

		setIsApplying(true);
		try {
			AccessibilityActionCreators.update({customThemeCss: css});
			ToastActionCreators.success(t`Theme applied successfully.`);
			ModalActionCreators.pop();
		} catch (error) {
			console.error('Failed to apply theme:', error);
			ToastActionCreators.error(t`We couldn't apply this theme.`);
			setIsApplying(false);
		}
	};

	const renderCodeContent = () => {
		if (fetchStatus === 'loading') {
			return (
				<span className={styles.loadingText}>
					<Trans>Loading theme...</Trans>
				</span>
			);
		}
		if (fetchStatus === 'error') {
			return <span className={styles.errorText}>{fetchError}</span>;
		}
		if (!css) {
			return null;
		}
		try {
			const highlighted = highlight.highlight(css, {language: 'css', ignoreIllegals: true});
			return (
				// biome-ignore lint/security/noDangerouslySetInnerHtml: highlight.js output is sanitized
				<code className={styles.hljs} dangerouslySetInnerHTML={{__html: highlighted.value}} />
			);
		} catch {
			return <code className={styles.hljs}>{css}</code>;
		}
	};

	return (
		<Modal.Root size="medium">
			<Modal.Header title={<Trans>Import theme</Trans>} />
			<Modal.Content padding="none" className={styles.content}>
				<p className={styles.description}>
					<Trans>This will replace your current custom theme. You can edit it later in your User Settings.</Trans>
				</p>
				<div className={styles.codeContainer}>
					<div className={clsx(styles.codeActions, isCopied && styles.codeActionsVisible)}>
						<button
							type="button"
							onClick={handleCopy}
							className={styles.copyButton}
							aria-label={isCopied ? t`Copied!` : t`Copy code`}
							disabled={!css}
						>
							{isCopied ? (
								<CheckCircleIcon className={styles.copyIcon} />
							) : (
								<ClipboardIcon className={styles.copyIcon} />
							)}
						</button>
					</div>
					<pre className={styles.pre}>{renderCodeContent()}</pre>
				</div>
			</Modal.Content>
			<Modal.Footer className={styles.footer}>
				<Button variant="secondary" onClick={handleDismiss} disabled={isApplying}>
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleApply} disabled={isApplying || fetchStatus !== 'ready'} submitting={isApplying}>
					<Trans>Apply</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
