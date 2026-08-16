// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import Updater, {
	DOWNLOADING_UPDATE_DESCRIPTOR as DOWNLOADING_UPDATE_STATUS_DESCRIPTOR,
} from '@app/features/app/state/Updater';
import styles from '@app/features/channel/components/ChannelHeader.module.css';
import {Platform} from '@app/features/platform/types/Platform';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon, DownloadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const INSTALLING_UPDATE_DESCRIPTOR = msg({
	message: 'Installing desktop update {version}…',
	comment:
		'Tooltip on the channel header updater icon while a Fluxer Desktop update is being installed. version is the target version. Trailing horizontal ellipsis is intentional.',
});
const INSTALLING_UPDATE_2_DESCRIPTOR = msg({
	message: 'Installing desktop update…',
	comment:
		'Tooltip on the channel header updater icon while a Fluxer Desktop update is being installed when version is not known. Trailing horizontal ellipsis is intentional.',
});
const DOWNLOADING_UPDATE_AT_S_DESCRIPTOR = msg({
	message: 'Downloading desktop update: {percent}% ({formatBytes} / {formatBytes2} at {formatBytes3}/s)',
	comment:
		'Tooltip on the channel header updater icon while downloading an update. Shows percent, bytes downloaded, total bytes, and bytes per second.',
});
const DOWNLOADING_UPDATE_DESCRIPTOR = msg({
	message: 'Downloading desktop update {version}: {percent}%',
	comment: 'Tooltip on the channel header updater icon while downloading a versioned update without throughput info.',
});
const DOWNLOADING_UPDATE_2_DESCRIPTOR = msg({
	message: 'Downloading desktop update: {percent}%',
	comment: 'Tooltip on the channel header updater icon while downloading an update with only percent available.',
});
const CLICK_TO_DOWNLOAD_UPDATE_DESCRIPTOR = msg({
	message: 'Download desktop update {version} ({formatBytes})',
	comment: 'Tooltip on the channel header updater icon prompting download of a versioned update with known size.',
});
const CLICK_TO_DOWNLOAD_UPDATE_2_DESCRIPTOR = msg({
	message: 'Download desktop update ({formatBytes})',
	comment:
		'Tooltip on the channel header updater icon prompting download of an update of known size but unknown version.',
});
const CLICK_TO_DOWNLOAD_UPDATE_3_DESCRIPTOR = msg({
	message: 'Download desktop update {version}',
	comment: 'Tooltip on the channel header updater icon prompting download of a versioned update with unknown size.',
});
const CLICK_TO_DOWNLOAD_UPDATE_4_DESCRIPTOR = msg({
	message: 'Download desktop update',
	comment: 'Tooltip on the channel header updater icon prompting download of an update with no version or size info.',
});
const CHOOSE_LINUX_PACKAGE_DESCRIPTOR = msg({
	message: 'Desktop update {version} available. Choose a Linux package.',
	comment: 'Tooltip on the updater icon for a Linux desktop update that requires choosing a package format.',
});
const CHOOSE_LINUX_PACKAGE_2_DESCRIPTOR = msg({
	message: 'Desktop update available. Choose a Linux package.',
	comment:
		'Tooltip on the updater icon for a Linux desktop update that requires choosing a package format and no version is known.',
});
const CLICK_TO_RESTART_AND_INSTALL_UPDATE_DESCRIPTOR = msg({
	message: 'Restart {productName} to install desktop update {version}',
	comment:
		'Tooltip on the channel header updater icon prompting a restart to apply a downloaded versioned update. productName is the app name.',
});
const CLICK_TO_RESTART_AND_INSTALL_UPDATE_2_DESCRIPTOR = msg({
	message: 'Restart {productName} to install desktop update',
	comment:
		'Tooltip on the channel header updater icon prompting a restart to apply a downloaded update of unknown version. productName is the app name.',
});
const CLICK_TO_RELOAD_AND_UPDATE_DESCRIPTOR = msg({
	message: 'Reload {productName} to use web update {version}',
	comment:
		'Tooltip on the channel header updater icon prompting a web reload to apply an in-place build update. productName is the app name.',
});
const CLICK_TO_RELOAD_AND_UPDATE_2_DESCRIPTOR = msg({
	message: 'Reload {productName} to use web update',
	comment:
		'Tooltip on the channel header updater icon prompting a web reload to apply an update of unknown version. productName is the app name.',
});

function formatBytes(bytes: number): string {
	if (!Number.isFinite(bytes) || bytes <= 0) return '0 B';
	const units = ['B', 'KB', 'MB', 'GB'];
	let value = bytes;
	let unitIndex = 0;
	while (value >= 1024 && unitIndex < units.length - 1) {
		value /= 1024;
		unitIndex++;
	}
	return `${value.toFixed(value >= 100 || unitIndex === 0 ? 0 : 1)} ${units[unitIndex]}`;
}

function resolveUpdaterVisibility(): {
	readonly hasActionableNativeUpdate: boolean;
	readonly hasActionableWebUpdate: boolean;
} {
	const hasActionableNativeUpdate =
		Platform.isElectron &&
		(Updater.nativeUpdateReady ||
			Updater.nativeAwaitingDownload ||
			Updater.nativeManualUpdateAvailable ||
			Updater.nativeDownloadInFlight);
	const hasActionableWebUpdate = !!Updater.updateInfo.web.available && !hasActionableNativeUpdate;
	return {hasActionableNativeUpdate, hasActionableWebUpdate};
}

export function isUpdaterIconVisible(): boolean {
	const {hasActionableNativeUpdate, hasActionableWebUpdate} = resolveUpdaterVisibility();
	return hasActionableNativeUpdate || hasActionableWebUpdate;
}

export const UpdaterIcon = observer(() => {
	const {i18n} = useLingui();
	const store = Updater;
	const {hasActionableNativeUpdate, hasActionableWebUpdate} = resolveUpdaterVisibility();
	const tooltip = useMemo(() => {
		const version = store.displayVersion;
		if (Platform.isElectron && store.updateInfo.native.installing) {
			return version ? i18n._(INSTALLING_UPDATE_DESCRIPTOR, {version}) : i18n._(INSTALLING_UPDATE_2_DESCRIPTOR);
		}
		if (Platform.isElectron && store.nativeDownloadInFlight) {
			if (!store.nativeDownloadProgressSupported) {
				return i18n._(DOWNLOADING_UPDATE_STATUS_DESCRIPTOR);
			}
			const progress = store.downloadProgress;
			const percent = progress ? Math.round(progress.percent) : 0;
			if (progress && progress.total > 0 && progress.bytesPerSecond > 0) {
				return i18n._(DOWNLOADING_UPDATE_AT_S_DESCRIPTOR, {
					percent,
					formatBytes: formatBytes(progress.transferred),
					formatBytes2: formatBytes(progress.total),
					formatBytes3: formatBytes(progress.bytesPerSecond),
				});
			}
			return version
				? i18n._(DOWNLOADING_UPDATE_DESCRIPTOR, {version, percent})
				: i18n._(DOWNLOADING_UPDATE_2_DESCRIPTOR, {percent});
		}
		if (Platform.isElectron && (store.nativeAwaitingDownload || store.nativeManualUpdateAvailable)) {
			if (store.nativeManualUpdateAvailable) {
				return version ? i18n._(CHOOSE_LINUX_PACKAGE_DESCRIPTOR, {version}) : i18n._(CHOOSE_LINUX_PACKAGE_2_DESCRIPTOR);
			}
			const size = store.updateInfo.native.downloadSize;
			if (version && size && size > 0) {
				return i18n._(CLICK_TO_DOWNLOAD_UPDATE_DESCRIPTOR, {version, formatBytes: formatBytes(size)});
			}
			if (size && size > 0) {
				return i18n._(CLICK_TO_DOWNLOAD_UPDATE_2_DESCRIPTOR, {formatBytes: formatBytes(size)});
			}
			return version
				? i18n._(CLICK_TO_DOWNLOAD_UPDATE_3_DESCRIPTOR, {version})
				: i18n._(CLICK_TO_DOWNLOAD_UPDATE_4_DESCRIPTOR);
		}
		if (Platform.isElectron && store.nativeUpdateReady) {
			return version
				? i18n._(CLICK_TO_RESTART_AND_INSTALL_UPDATE_DESCRIPTOR, {version, productName: PRODUCT_NAME})
				: i18n._(CLICK_TO_RESTART_AND_INSTALL_UPDATE_2_DESCRIPTOR, {productName: PRODUCT_NAME});
		}
		return version
			? i18n._(CLICK_TO_RELOAD_AND_UPDATE_DESCRIPTOR, {version, productName: PRODUCT_NAME})
			: i18n._(CLICK_TO_RELOAD_AND_UPDATE_2_DESCRIPTOR, {productName: PRODUCT_NAME});
	}, [
		store.displayVersion,
		store.downloadProgress,
		store.nativeAwaitingDownload,
		store.nativeDownloadInFlight,
		store.nativeDownloadProgressSupported,
		store.nativeManualUpdateAvailable,
		store.nativeUpdateReady,
		store.updateInfo.native.downloadSize,
		store.updateInfo.native.installing,
		i18n.locale,
	]);
	const handleClick = useCallback(() => {
		void store.applyUpdate();
	}, [store]);
	if (!hasActionableNativeUpdate && !hasActionableWebUpdate) {
		return null;
	}
	const isInstalling = Platform.isElectron && store.updateInfo.native.installing;
	const isDownloading = Platform.isElectron && store.nativeDownloadInFlight;
	const buttonClass = isInstalling ? styles.updateIconButtonDisabled : styles.updateIconButton;
	const Icon = hasActionableWebUpdate ? ArrowClockwiseIcon : DownloadSimpleIcon;
	const percentLabel =
		isDownloading && store.nativeDownloadProgressSupported
			? `${Math.round(store.downloadProgress?.percent ?? 0)}%`
			: null;
	return (
		<Tooltip text={tooltip} position="bottom" data-flx="channel.channel-header-components.updater-icon.tooltip">
			<FocusRing offset={-2} data-flx="channel.channel-header-components.updater-icon.focus-ring">
				<button
					type="button"
					className={buttonClass}
					onClick={handleClick}
					aria-label={tooltip}
					aria-disabled={isInstalling || undefined}
					data-flx="channel.channel-header-components.updater-icon.button.click"
				>
					<Icon
						weight="bold"
						className={styles.updateIcon}
						data-flx="channel.channel-header-components.updater-icon.update-icon"
					/>
					{percentLabel != null && (
						<span
							className={styles.updateProgress}
							data-flx="channel.channel-header-components.updater-icon.update-progress"
						>
							{percentLabel}
						</span>
					)}
				</button>
			</FocusRing>
		</Tooltip>
	);
});
