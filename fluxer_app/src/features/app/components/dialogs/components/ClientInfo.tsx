// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/components/ClientInfo.module.css';
import Config from '@app/features/app/config/Config';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import {UNKNOWN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	formatClientBuildInfo,
	formatReleaseChannelLabel,
	getClientInfo,
	getClientInfoSync,
} from '@app/features/platform/utils/ClientInfo';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const WEB_BUILD_DESCRIPTOR = msg({
	message: '{releaseChannel} Web {buildVersion}',
	comment: 'Client info web build identifier line. Example: "Canary Web 2026.518.0". Preserve placeholders.',
});
const WEB_BUILD_SECONDARY_DESCRIPTOR = msg({
	message: 'Web {buildVersion}',
	comment:
		'Client info web build identifier shown after the desktop build when both are present. Example: "Web 2026.518.0". Preserve placeholder.',
});
const WEB_BUILD_SECONDARY_WITH_CHANNEL_DESCRIPTOR = msg({
	message: '{releaseChannel} Web {buildVersion}',
	comment:
		'Client info web build identifier shown after the desktop build when the web and desktop channels differ. Example: "Stable Web 2026.518.0". Preserve placeholders.',
});
const DESKTOP_BUILD_DESCRIPTOR = msg({
	message: '{desktopChannel} Desktop {desktopVersion}',
	comment: 'Client info desktop build identifier line. Example: "Canary Desktop 2026.419.3". Preserve placeholders.',
});
const YOU_ARE_NOW_A_DEVELOPER_DESCRIPTOR = msg({
	message: 'You are now a developer!',
	comment: 'Body text in the settings dialog client info.',
});
const CLICK_TO_COPY_DESCRIPTOR = msg({
	message: 'Click to copy',
	comment: 'Short label in the settings dialog client info.',
});
export const ClientInfo = observer(() => {
	const {i18n} = useLingui();
	const [clientInfo, setClientInfo] = useState(getClientInfoSync());
	useEffect(() => {
		let mounted = true;
		void getClientInfo().then((info) => {
			if (!mounted) return;
			setClientInfo(info);
		});
		return () => {
			mounted = false;
		};
	}, []);
	const desktopVersion = clientInfo.desktopVersion;
	const desktopChannel = clientInfo.desktopChannel;
	const unknownLabel = i18n._(UNKNOWN_DESCRIPTOR);
	const browserName = clientInfo.browserName || unknownLabel;
	const browserVersion = clientInfo.browserVersion || '';
	const osName = clientInfo.osName || unknownLabel;
	const rawOsVersion = clientInfo.osVersion ?? '';
	const isDesktopApp = isDesktop();
	const osArchitecture = clientInfo.desktopArch ?? clientInfo.arch;
	const shouldShowOsVersion = Boolean(rawOsVersion) && (isDesktopApp || osName !== 'macOS');
	const osVersionForDisplay = shouldShowOsVersion ? rawOsVersion : undefined;
	const buildOsDescription = () => {
		const parts = [osName];
		if (osVersionForDisplay) {
			parts.push(osVersionForDisplay);
		}
		const archSuffix = osArchitecture ? ` (${osArchitecture})` : '';
		return `${parts.join(' ')}${archSuffix}`.trim();
	};
	const osDescription = buildOsDescription();
	const releaseChannel = formatReleaseChannelLabel(Config.PUBLIC_RELEASE_CHANNEL);
	const buildVersion = Config.PUBLIC_BUILD_VERSION || 'dev';
	const desktopReleaseChannel = desktopChannel ? formatReleaseChannelLabel(desktopChannel) : null;
	const primaryDesktopReleaseChannel = desktopReleaseChannel ?? releaseChannel;
	const desktopVersionLabel = desktopVersion;
	const desktopBuildLabel = desktopVersion
		? i18n._(DESKTOP_BUILD_DESCRIPTOR, {
				desktopChannel: primaryDesktopReleaseChannel,
				desktopVersion: desktopVersionLabel,
			})
		: null;
	const buildLabel = desktopBuildLabel
		? primaryDesktopReleaseChannel === releaseChannel
			? i18n._(WEB_BUILD_SECONDARY_DESCRIPTOR, {buildVersion})
			: i18n._(WEB_BUILD_SECONDARY_WITH_CHANNEL_DESCRIPTOR, {releaseChannel, buildVersion})
		: i18n._(WEB_BUILD_DESCRIPTOR, {
				releaseChannel,
				buildVersion,
			});
	const onClick = () => {
		const justUnlocked = DeveloperMode.registerBuildTap();
		if (justUnlocked) {
			ToastCommands.success(i18n._(YOU_ARE_NOW_A_DEVELOPER_DESCRIPTOR));
		}
		TextCopyCommands.copy(i18n, formatClientBuildInfo(clientInfo, {unknownLabel}));
	};
	return (
		<Tooltip text={i18n._(CLICK_TO_COPY_DESCRIPTOR)} data-flx="app.client-info.tooltip">
			<FocusRing data-flx="app.client-info.focus-ring">
				<button type="button" onClick={onClick} className={styles.button} data-flx="app.client-info.button.click">
					{desktopBuildLabel && <span data-flx="app.client-info.span">{desktopBuildLabel}</span>}
					<span data-flx="app.client-info.span--2">{buildLabel}</span>
					<span data-flx="app.client-info.span--4">
						{browserName} {browserVersion}
					</span>
					<span data-flx="app.client-info.span--5">{osDescription}</span>
				</button>
			</FocusRing>
		</Tooltip>
	);
});
