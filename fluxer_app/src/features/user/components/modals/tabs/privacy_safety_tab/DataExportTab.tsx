// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/DataDeletionTab.module.css';
import {
	DataRequestModal,
	EXPORT_TAB_DESCRIPTION,
} from '@app/features/user/components/modals/tabs/privacy_safety_tab/data_request_modal/DataRequestModal';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as FormUtils from '@app/lib/forms';
import type {HarvestStatusResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const HARVEST_POLL_INTERVAL_MS = 10_000;

const DOWNLOAD_FAILED_DESCRIPTOR = msg({
	message: 'Could not download your data export',
	comment: 'Error title shown when fetching the download link for a finished data export fails.',
});

function isHarvestInProgress(harvest: HarvestStatusResponse | null): boolean {
	return harvest?.status === 'pending' || harvest?.status === 'processing';
}

function isHarvestDownloadable(harvest: HarvestStatusResponse): boolean {
	if (harvest.status !== 'completed') return false;
	return !harvest.expires_at || new Date(harvest.expires_at).getTime() > Date.now();
}

const DataExportStatus = observer(
	({
		harvest,
		onDownload,
		downloading,
	}: {
		harvest: HarvestStatusResponse;
		onDownload: () => void;
		downloading: boolean;
	}) => {
		const {i18n} = useLingui();
		const requestedAt = DateUtils.getRelativeDateString(harvest.created_at, i18n);
		if (isHarvestInProgress(harvest)) {
			const progress = Math.round(harvest.progress_percent);
			return (
				<Modal.Description className={styles.warningText} data-flx="user.privacy-safety-tab.data-export-tab.status">
					<Trans>
						Your export from {requestedAt} is being prepared ({progress}%).
					</Trans>
				</Modal.Description>
			);
		}
		if (harvest.status === 'failed') {
			return (
				<Modal.Description className={styles.warningText} data-flx="user.privacy-safety-tab.data-export-tab.status">
					<Trans>Your export from {requestedAt} failed. You can request a new one.</Trans>
				</Modal.Description>
			);
		}
		if (!isHarvestDownloadable(harvest)) {
			return (
				<Modal.Description className={styles.warningText} data-flx="user.privacy-safety-tab.data-export-tab.status">
					<Trans>Your export from {requestedAt} has expired. You can request a new one.</Trans>
				</Modal.Description>
			);
		}
		const expiresAt = harvest.expires_at ? DateUtils.getFormattedDateTime(harvest.expires_at) : null;
		return (
			<>
				<Modal.Description className={styles.warningText} data-flx="user.privacy-safety-tab.data-export-tab.status">
					{expiresAt ? (
						<Trans>
							Your export from {requestedAt} is ready. The download expires on {expiresAt}.
						</Trans>
					) : (
						<Trans>Your export from {requestedAt} is ready.</Trans>
					)}
				</Modal.Description>
				<Button
					variant="secondary"
					submitting={downloading}
					onClick={onDownload}
					data-flx="user.privacy-safety-tab.data-export-tab.button.download"
				>
					<Trans>Download export</Trans>
				</Button>
			</>
		);
	},
);

export const DataExportTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const showInAppStatus = RuntimeConfig.usesUsernameSignIn;
	const [harvest, setHarvest] = useState<HarvestStatusResponse | null>(null);
	const [refreshToken, setRefreshToken] = useState(0);
	const [downloading, setDownloading] = useState(false);
	const inProgress = isHarvestInProgress(harvest);
	useEffect(() => {
		if (!showInAppStatus) return;
		let cancelled = false;
		let timer: number | null = null;
		const load = async () => {
			try {
				const latest = await UserCommands.getLatestHarvest();
				if (cancelled) return;
				setHarvest(latest);
				if (isHarvestInProgress(latest)) {
					timer = window.setTimeout(load, HARVEST_POLL_INTERVAL_MS);
				}
			} catch {}
		};
		void load();
		return () => {
			cancelled = true;
			if (timer !== null) window.clearTimeout(timer);
		};
	}, [showInAppStatus, refreshToken]);
	const handleExportRequested = useCallback(() => setRefreshToken((value) => value + 1), []);
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<DataRequestModal
					variant="export"
					onExportRequested={handleExportRequested}
					data-flx="user.privacy-safety-tab.data-export-tab.data-request-modal"
				/>
			)),
		);
	}, [handleExportRequested]);
	const handleDownload = useCallback(async () => {
		if (!harvest) return;
		setDownloading(true);
		try {
			const {download_url: downloadUrl} = await UserCommands.getHarvestDownloadUrl(harvest.harvest_id);
			await openExternalUrl(downloadUrl);
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error, i18n._(DOWNLOAD_FAILED_DESCRIPTOR));
		} finally {
			setDownloading(false);
		}
	}, [harvest, i18n]);
	return (
		<div
			className={styles.deleteSection}
			data-flx="user.privacy-safety-tab.data-export-tab.data-export-tab-content.delete-section"
		>
			<Modal.Description
				className={styles.warningText}
				data-flx="user.privacy-safety-tab.data-export-tab.data-export-tab-content.warning-text"
			>
				{i18n._(EXPORT_TAB_DESCRIPTION)}
			</Modal.Description>
			{showInAppStatus && harvest && (
				<DataExportStatus
					harvest={harvest}
					onDownload={handleDownload}
					downloading={downloading}
					data-flx="user.privacy-safety-tab.data-export-tab.data-export-tab-content.status"
				/>
			)}
			<Button
				variant="primary"
				disabled={showInAppStatus && inProgress}
				onClick={handleOpen}
				data-flx="user.privacy-safety-tab.data-export-tab.data-export-tab-content.button.open-modal"
			>
				<Trans>Export my data</Trans>
			</Button>
		</div>
	);
});
