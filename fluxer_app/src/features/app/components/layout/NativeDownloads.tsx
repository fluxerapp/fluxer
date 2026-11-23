// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NativeDownloads.module.css';
import DownloadsStore, {isDownloadInProgress} from '@app/features/app/state/DownloadsStore';
import {formatNumber} from '@app/features/i18n/utils/IntlCache';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {formatRoundedPercentage} from '@app/features/ui/utils/PercentageFormatting';
import type {DownloadEntryEvent} from '@app/types/electron';
import {NATIVE_TITLEBAR_CLASS} from '@fluxer/desktop_ipc/src/NativeTitlebarShell';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon, FolderOpenIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

const DOWNLOADS_POPOUT_ID = 'native-titlebar-downloads';

const DOWNLOADS_DESCRIPTOR = msg({
	message: 'View downloads',
	comment: 'Button in the app layout native titlebar that opens the downloads panel.',
});
const DOWNLOADS_ACTIVE_DESCRIPTOR = msg({
	message: 'View downloads. {count, plural, one {# download in progress} other {# downloads in progress}}',
	comment:
		'Screen reader label of the titlebar button that opens the downloads panel, used while downloads are running. {count} is the number of downloads in progress.',
});
const DOWNLOADS_TITLE_DESCRIPTOR = msg({
	message: 'Downloads',
	comment: 'Heading of the downloads panel.',
});
const CLEAR_LIST_DESCRIPTOR = msg({
	message: 'Clear list',
	comment:
		'Removes finished downloads from the downloads panel. Downloaded files stay on disk and in-progress downloads stay in the list.',
});
const SHOW_IN_FOLDER_DESCRIPTOR = msg({
	message: 'Show in folder',
	comment: 'Opens the folder containing a downloaded file.',
});
const CANCEL_DOWNLOAD_DESCRIPTOR = msg({
	message: 'Cancel download',
	comment: 'Stops an in-progress download and deletes the partial file.',
});
const REMOVE_DOWNLOAD_DESCRIPTOR = msg({
	message: 'Remove from list',
	comment: 'Removes a finished download from the downloads panel.',
});
const DOWNLOAD_COMPLETED_DESCRIPTOR = msg({
	message: 'Completed',
	comment:
		'Status shown under a file name in the downloads panel once the file has finished downloading. Keep it short.',
});
const DOWNLOAD_CANCELLED_DESCRIPTOR = msg({
	message: 'Cancelled. The partial file was deleted.',
	comment:
		'Status shown under a file name in the downloads panel after the user cancels the download. The part of the file that had already downloaded is deleted.',
});
const DOWNLOAD_FILE_MISSING_DESCRIPTOR = msg({
	message: 'The file was moved or deleted.',
	comment:
		'Status shown under a file name in the downloads panel when the download finished but the file is no longer where it was saved.',
});
const DOWNLOAD_FAILED_DESCRIPTOR = msg({
	message: 'Failed',
	comment: 'Status shown under a file name in the downloads panel when the download did not finish. Keep it short.',
});
const DOWNLOAD_STARTING_DESCRIPTOR = msg({
	message: 'Starting',
	comment:
		'Status shown under a file name in the downloads panel while the download is starting and no data has arrived yet. Keep it short.',
});
const DOWNLOAD_PROGRESS_DESCRIPTOR = msg({
	message: '{progress} of {total}',
	comment:
		'Download progress shown under a file name in the downloads panel. {progress} is the formatted percentage downloaded so far, for example 42%. {total} is the formatted size of the whole file, for example 3.2 MB.',
});

export const NativeDownloads: React.FC = observer(() => {
	const {i18n} = useLingui();
	const displayDownload = (entry: DownloadEntryEvent): string => {
		switch (entry.state) {
			case 'completed':
				return i18n._(entry.fileExists ? DOWNLOAD_COMPLETED_DESCRIPTOR : DOWNLOAD_FILE_MISSING_DESCRIPTOR);
			case 'canceled':
				return i18n._(DOWNLOAD_CANCELLED_DESCRIPTOR);
			case 'failed':
				return i18n._(DOWNLOAD_FAILED_DESCRIPTOR);
			case 'started':
				return i18n._(DOWNLOAD_STARTING_DESCRIPTOR);
			default:
				if (entry.totalBytes != null && entry.totalBytes > 0) {
					return i18n._(DOWNLOAD_PROGRESS_DESCRIPTOR, {
						progress: formatRoundedPercentage(i18n.locale, (entry.receivedBytes / entry.totalBytes) * 100),
						total: formatFileSize(i18n.locale, entry.totalBytes),
					});
				}
				return formatFileSize(i18n.locale, entry.receivedBytes);
		}
	};
	const {openProps} = usePopout(DOWNLOADS_POPOUT_ID);
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const popoutRef = useRef<HTMLDivElement | null>(null);
	if (!DownloadsStore.hasEntries) {
		return null;
	}
	const activeCount = DownloadsStore.activeCount;
	return (
		<Popout
			data-flx="app.native-downloads.popout"
			{...openProps}
			position="bottom-end"
			offsetMainAxis={1}
			ignoreTitlebarBoundary
			subscribeTo="DOWNLOADS_POPOUT_OPEN"
			onOpen={DownloadsStore.load}
			returnFocusRef={buttonRef}
			render={() => (
				<FocusRingScope containerRef={popoutRef} data-flx="app.native-downloads.focus-ring-scope">
					<div
						ref={popoutRef}
						role="dialog"
						aria-label={i18n._(DOWNLOADS_TITLE_DESCRIPTOR)}
						className={styles.panel}
						data-flx="app.native-downloads.panel"
					>
						<div className={styles.header} data-flx="app.native-downloads.header">
							<span className={styles.title} data-flx="app.native-downloads.title">
								{i18n._(DOWNLOADS_TITLE_DESCRIPTOR)}
							</span>
							<FocusRing offset={-2} data-flx="app.native-downloads.focus-ring">
								<button
									type="button"
									className={styles.clearList}
									onClick={DownloadsStore.clearList}
									data-flx="app.native-downloads.clear-list.button"
								>
									{i18n._(CLEAR_LIST_DESCRIPTOR)}
								</button>
							</FocusRing>
						</div>
						<ul className={styles.list} data-flx="app.native-downloads.list">
							{DownloadsStore.entries.map((entry) => {
								const isDownloading = isDownloadInProgress(entry);
								const canOpen = entry.state === 'completed' && entry.fileExists;
								return (
									<li key={entry.id} className={styles.item} data-flx="app.native-downloads.item">
										<FocusRing offset={-2} data-flx="app.native-downloads.focus-ring--2">
											<button
												type="button"
												className={styles.itemButton}
												onClick={() => DownloadsStore.open(entry)}
												disabled={!canOpen}
												data-flx="app.native-downloads.item-button.open"
											>
												<span className={styles.itemBody} data-flx="app.native-downloads.item-body">
													<span className={styles.itemName} data-flx="app.native-downloads.item-name">
														{entry.fileName}
													</span>
													<span className={styles.itemMeta} data-flx="app.native-downloads.item-meta">
														{displayDownload(entry)}
													</span>
												</span>
											</button>
										</FocusRing>
										<div className={styles.itemControls} data-flx="app.native-downloads.item-controls">
											{canOpen && (
												<FocusRing offset={-2} data-flx="app.native-downloads.focus-ring--3">
													<button
														type="button"
														className={styles.cancelOrDismiss}
														onClick={() => DownloadsStore.reveal(entry)}
														aria-label={i18n._(SHOW_IN_FOLDER_DESCRIPTOR)}
														data-flx="app.native-downloads.cancel-or-dismiss.reveal.button"
													>
														<FolderOpenIcon size={20} weight="bold" data-flx="app.native-downloads.folder-open-icon" />
													</button>
												</FocusRing>
											)}
											<FocusRing offset={-2} data-flx="app.native-downloads.focus-ring--4">
												<button
													type="button"
													className={clsx(styles.cancelOrDismiss, isDownloading && styles.cancelOrDismissDanger)}
													onClick={() => (isDownloading ? DownloadsStore.cancel(entry) : DownloadsStore.dismiss(entry))}
													aria-label={
														isDownloading ? i18n._(CANCEL_DOWNLOAD_DESCRIPTOR) : i18n._(REMOVE_DOWNLOAD_DESCRIPTOR)
													}
													data-flx="app.native-downloads.cancel-or-dismiss.button"
												>
													<XIcon size={20} weight="bold" data-flx="app.native-downloads.x-icon" />
												</button>
											</FocusRing>
										</div>
									</li>
								);
							})}
						</ul>
					</div>
				</FocusRingScope>
			)}
		>
			<FocusRing offset={0} data-flx="app.native-downloads.focus-ring">
				<button
					ref={buttonRef}
					type="button"
					className={clsx(NATIVE_TITLEBAR_CLASS.action, styles.trigger)}
					aria-label={
						activeCount > 0 ? i18n._(DOWNLOADS_ACTIVE_DESCRIPTOR, {count: activeCount}) : i18n._(DOWNLOADS_DESCRIPTOR)
					}
					data-flx="app.native-downloads.trigger.button"
				>
					<DownloadSimpleIcon weight="bold" data-flx="app.native-downloads.download-simple-icon" />
					{activeCount > 0 && (
						<span className={styles.badge} aria-hidden="true" data-flx="app.native-downloads.badge">
							{formatNumber(activeCount, i18n.locale)}
						</span>
					)}
				</button>
			</FocusRing>
		</Popout>
	);
});
