// SPDX-License-Identifier: AGPL-3.0-or-later

import errorFallbackStyles from '@app/features/app/components/ErrorFallback.module.css';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import AppStorage, {PRESERVED_RESET_STORAGE_KEYS} from '@app/features/platform/state/PersistentStorage';
import {ensureLatestAssets} from '@app/features/platform/types/Versioning';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import {copy as copyText} from '@app/features/ui/commands/TextCopyCommands';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import LayerManager from '@app/features/ui/state/LayerManager';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

interface ErrorFallbackProps {
	error?: Error;
	reset?: () => void;
}

const logger = new Logger('ErrorFallback');
const UNKNOWN_ERROR_DESCRIPTOR = msg({
	message: 'Unknown error',
	comment: 'Fallback diagnostic text on the crash screen when an Error object has no message.',
});

function getStackTraceText(error: Error | undefined, unknownErrorLabel: string): string | null {
	if (!error) {
		return null;
	}
	const stack = error.stack?.trim();
	if (stack) {
		return stack;
	}
	const name = error.name?.trim() || 'Error';
	const message = error.message?.trim() || unknownErrorLabel;
	return `${name}: ${message}`;
}

async function cleanupRuntimeStateOnCrash(): Promise<void> {
	LayerManager.closeAll();
	const [{default: GatewayConnection}, {default: MediaEngine}] = await Promise.all([
		import('@app/features/gateway/transport/GatewayConnection'),
		import('@app/features/voice/engine/MediaEngineFacade'),
	]);
	GatewayConnection.logout();
	MediaEngine.cleanup();
}

export const ErrorFallback: React.FC<ErrorFallbackProps> = ({error}) => {
	const {i18n} = useLingui();
	const {platform, isNative} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const [updateAvailable, setUpdateAvailable] = useState(false);
	const [isUpdating, setIsUpdating] = useState(false);
	const [isCopyingStackTrace, setIsCopyingStackTrace] = useState(false);
	const [checkingForUpdates, setCheckingForUpdates] = useState(true);
	const stackTraceText = getStackTraceText(error, i18n._(UNKNOWN_ERROR_DESCRIPTOR));
	useEffect(() => {
		void cleanupRuntimeStateOnCrash().catch((error) => {
			logger.error('Failed to clean up runtime state on crash screen', error);
		});
	}, []);
	useEffect(() => {
		let isMounted = true;
		const run = async () => {
			try {
				const {updateFound} = await ensureLatestAssets({force: true});
				if (isMounted) {
					setUpdateAvailable(updateFound);
				}
			} catch (error) {
				logger.error('Failed to check for updates:', error);
			} finally {
				if (isMounted) {
					setCheckingForUpdates(false);
				}
			}
		};
		void run();
		return () => {
			isMounted = false;
		};
	}, []);
	const handleUpdate = useCallback(async () => {
		setIsUpdating(true);
		try {
			const {updateFound} = await ensureLatestAssets({force: true});
			if (!updateFound) {
				setIsUpdating(false);
				window.location.reload();
			}
		} catch (error) {
			logger.error('Failed to apply update:', error);
			setIsUpdating(false);
		}
	}, []);
	const handleCopyStackTrace = useCallback(async () => {
		if (!stackTraceText) {
			return;
		}
		setIsCopyingStackTrace(true);
		try {
			await copyText(i18n, stackTraceText);
		} finally {
			setIsCopyingStackTrace(false);
		}
	}, [stackTraceText]);
	return (
		<div className={errorFallbackStyles.errorFallbackContainer} data-flx="app.error-fallback.div">
			{isNative && !useSystemTitleBar && (
				<NativeTitlebar platform={platform} data-flx="app.error-fallback.native-titlebar" />
			)}
			<FluxerIcon className={errorFallbackStyles.errorFallbackIcon} data-flx="app.error-fallback.fluxer-icon" />
			<div className={errorFallbackStyles.errorFallbackContent} data-flx="app.error-fallback.div--2">
				<h1 className={errorFallbackStyles.errorFallbackTitle} data-flx="app.error-fallback.h1">
					<Trans>The app crashed</Trans>
				</h1>
				<p className={errorFallbackStyles.errorFallbackDescription} data-flx="app.error-fallback.p">
					{checkingForUpdates ? (
						<Trans>The app has crashed. Checking for updates that might fix this issue…</Trans>
					) : updateAvailable ? (
						<Trans>Something went wrong and the app crashed. An update is available that may fix this issue.</Trans>
					) : (
						<Trans>Something went wrong and the app crashed. Try reloading or resetting the app.</Trans>
					)}
				</p>
			</div>
			<div className={errorFallbackStyles.errorFallbackActions} data-flx="app.error-fallback.div--3">
				<Button
					onClick={updateAvailable ? handleUpdate : () => location.reload()}
					disabled={checkingForUpdates}
					submitting={isUpdating}
					data-flx="app.error-fallback.button.update"
				>
					{checkingForUpdates || updateAvailable ? (
						<Trans comment="Button on the crash screen that applies a found app update.">Update app</Trans>
					) : (
						<Trans comment="Button on the crash screen that reloads the current app.">Reload app</Trans>
					)}
				</Button>
				{stackTraceText && (
					<Button
						onClick={handleCopyStackTrace}
						submitting={isCopyingStackTrace}
						variant="secondary"
						data-flx="app.error-fallback.button.copy-stack-trace"
					>
						<Trans comment="Button on the crash screen that copies developer diagnostic text.">Copy stack trace</Trans>
					</Button>
				)}
				<Button
					onClick={() => {
						AppStorage.clearExcept(PRESERVED_RESET_STORAGE_KEYS);
						location.reload();
					}}
					variant="danger"
					disabled={checkingForUpdates}
					data-flx="app.error-fallback.button.clear-except"
				>
					<Trans comment="Destructive button on the crash screen. Clears local app data except preserved drafts.">
						Reset app data
					</Trans>
				</Button>
			</div>
		</div>
	);
};
