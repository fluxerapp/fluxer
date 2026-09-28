// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/ErrorFallback.module.css';
import {
	BLUESKY_PROVIDER_NAME,
	FLUXER_BLUESKY_HANDLE,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import AppStorage, {
	PRESERVED_RESET_STORAGE_KEY_PREFIXES,
	PRESERVED_RESET_STORAGE_KEYS,
} from '@app/features/platform/state/PersistentStorage';
import {Button} from '@app/features/ui/button/Button';
import {APPLICATION_ICON_DESCRIPTOR, FluxerIconMark} from '@app/features/ui/components/icons/FluxerIconMark';
import {ExternalUrls} from '@fluxer/constants/src/ExternalUrls';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback} from 'react';

interface BootstrapErrorScreenProps {
	error?: Error;
}

export const BootstrapErrorScreen: React.FC<BootstrapErrorScreenProps> = ({error}) => {
	const {i18n} = useLingui();
	const handleRetry = useCallback(() => {
		window.location.reload();
	}, []);
	const handleReset = useCallback(() => {
		AppStorage.clearExcept(PRESERVED_RESET_STORAGE_KEYS, PRESERVED_RESET_STORAGE_KEY_PREFIXES);
		window.location.reload();
	}, []);
	return (
		<div className={styles.errorFallbackContainer} data-flx="app.bootstrap-error-screen.error-fallback-container">
			<FluxerIconMark
				aria-label={i18n._(APPLICATION_ICON_DESCRIPTOR, {productName: PRODUCT_NAME})}
				className={styles.errorFallbackIcon}
				data-flx="app.bootstrap-error-screen.error-fallback-icon"
			/>
			<div className={styles.errorFallbackContent} data-flx="app.bootstrap-error-screen.error-fallback-content">
				<h1 className={styles.errorFallbackTitle} data-flx="app.bootstrap-error-screen.error-fallback-title">
					<Trans>Failed to start</Trans>
				</h1>
				<p className={styles.errorFallbackDescription} data-flx="app.bootstrap-error-screen.error-fallback-description">
					<Trans>
						{PRODUCT_NAME} failed to start properly. This could be due to corrupted data or a temporary issue.
					</Trans>
				</p>
				{error && (
					<p
						className={styles.errorFallbackDescription}
						style={{fontSize: '0.875rem', opacity: 0.8}}
						data-flx="app.bootstrap-error-screen.error-fallback-description--2"
					>
						{error.message}
					</p>
				)}
				<p
					className={styles.errorFallbackDescription}
					data-flx="app.bootstrap-error-screen.error-fallback-description--3"
				>
					<Trans>
						Check our{' '}
						<a
							href={ExternalUrls.BLUESKY}
							target="_blank"
							rel="noopener noreferrer"
							data-flx="app.bootstrap-error-screen.a"
						>
							{BLUESKY_PROVIDER_NAME} ({FLUXER_BLUESKY_HANDLE})
						</a>{' '}
						for status updates.
					</Trans>
				</p>
			</div>
			<div className={styles.errorFallbackActions} data-flx="app.bootstrap-error-screen.error-fallback-actions">
				<Button onClick={handleRetry} data-flx="app.bootstrap-error-screen.button.retry">
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
				<Button onClick={handleReset} variant="danger" data-flx="app.bootstrap-error-screen.button.reset">
					<Trans comment="Destructive button on the startup failure screen. Clears local app data except preserved drafts.">
						Reset app data
					</Trans>
				</Button>
			</div>
		</div>
	);
};
