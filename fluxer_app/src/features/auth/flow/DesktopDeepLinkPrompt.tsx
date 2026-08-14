// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/auth/flow/DesktopDeepLinkPrompt.module.css';
import {Platform} from '@app/features/platform/types/Platform';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {buildAppProtocolUrl} from '@app/features/ui/utils/AppProtocol';
import {isDesktop, openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {checkDesktopAvailable, navigateInDesktop} from '@app/features/voice/utils/DesktopRpcClient';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useEffect, useState} from 'react';

interface DesktopDeepLinkPromptProps {
	code: string;
	kind: 'invite' | 'gift' | 'theme';
	preferLogin?: boolean;
}

const OPEN_IN_DESKTOP_PRODUCT_DESCRIPTOR = msg({
	message: 'Open in {productName} for desktop',
	comment: 'Title in a web prompt that opens the same flow in the desktop app. productName is the app name.',
});
const OPEN_PRODUCT_DESCRIPTOR = msg({
	message: 'Open {productName}',
	comment: 'Button label that opens the desktop app. productName is the app name.',
});
const FAILED_TO_OPEN_IN_DESKTOP_APP_DESCRIPTOR = msg({
	message: 'Something went wrong. Try again.',
	comment: 'Inline error in the web deep-link prompt when opening the desktop app fails.',
});
export const DesktopDeepLinkPrompt: React.FC<DesktopDeepLinkPromptProps> = ({code, kind, preferLogin = false}) => {
	const {i18n} = useLingui();
	const [isLoading, setIsLoading] = useState(false);
	const [desktopAvailable, setDesktopAvailable] = useState<boolean | null>(null);
	const [error, setError] = useState<string | null>(null);
	const isMobileBrowser = Platform.isMobileBrowser;
	const useProtocolLaunch = kind === 'invite';
	const shouldProbeDesktopAvailability = !useProtocolLaunch;
	useEffect(() => {
		if (isDesktop() || !shouldProbeDesktopAvailability) return;
		let cancelled = false;
		checkDesktopAvailable().then(({available}) => {
			if (!cancelled) {
				setDesktopAvailable(available);
			}
		});
		return () => {
			cancelled = true;
		};
	}, [shouldProbeDesktopAvailability]);
	if (isDesktop() || isMobileBrowser) return null;
	if (shouldProbeDesktopAvailability && desktopAvailable !== true) return null;
	const getPath = (): string => {
		switch (kind) {
			case 'invite':
				return preferLogin ? Routes.inviteLogin(code) : Routes.inviteRegister(code);
			case 'gift':
				return preferLogin ? Routes.giftLogin(code) : Routes.giftRegister(code);
			case 'theme':
				return preferLogin ? Routes.themeLogin(code) : Routes.themeRegister(code);
		}
	};
	const path = getPath();
	const handleOpen = async () => {
		setIsLoading(true);
		setError(null);
		if (useProtocolLaunch) {
			try {
				await openExternalUrl(buildAppProtocolUrl(path));
			} catch {
				setError(i18n._(FAILED_TO_OPEN_IN_DESKTOP_APP_DESCRIPTOR));
			} finally {
				setIsLoading(false);
			}
			return;
		}
		const result = await navigateInDesktop(path);
		setIsLoading(false);
		if (!result.success) {
			setError(i18n._(FAILED_TO_OPEN_IN_DESKTOP_APP_DESCRIPTOR));
		}
	};
	return (
		<div className={styles.banner} data-flx="auth.flow.desktop-deep-link-prompt.banner">
			<div className={styles.copy} data-flx="auth.flow.desktop-deep-link-prompt.copy">
				<p className={styles.title} data-flx="auth.flow.desktop-deep-link-prompt.title">
					{i18n._(OPEN_IN_DESKTOP_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</p>
				{error ? (
					<p className={styles.notInstalled} data-flx="auth.flow.desktop-deep-link-prompt.not-installed">
						{error}
					</p>
				) : (
					<p className={styles.body} data-flx="auth.flow.desktop-deep-link-prompt.body">
						<Trans>Jump straight to the app to continue.</Trans>
					</p>
				)}
			</div>
			<Button
				variant="primary"
				onClick={handleOpen}
				className={styles.cta}
				submitting={isLoading}
				data-flx="auth.flow.desktop-deep-link-prompt.cta.open"
			>
				<ArrowSquareOutIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="auth.flow.desktop-deep-link-prompt.arrow-square-out-icon"
				/>
				<span data-flx="auth.flow.desktop-deep-link-prompt.span">
					{i18n._(OPEN_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</span>
			</Button>
		</div>
	);
};
