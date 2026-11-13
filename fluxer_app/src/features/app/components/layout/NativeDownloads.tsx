// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NativeDownloads.module.css';
import DownloadsStore from '@app/features/app/state/DownloadsStore';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import type {DownloadEntryEvent} from '@app/types/electron';
import {
	NATIVE_TITLEBAR_CLASS,
	NATIVE_TITLEBAR_DOWNLOADS_ICON_PATHS,
	NATIVE_TITLEBAR_DOWNLOADS_ICON_VIEWBOX,
} from '@fluxer/desktop_ipc/src/NativeTitlebarShell';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {BroomIcon, DownloadSimpleIcon, FolderOpenIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useLayoutEffect, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

const DOWNLOADS_POPOUT_ID = 'native-titlebar-downloads';
const ARC_DURATION_MS = 700;
const ORIGIN_MAX_AGE_MS = 60000;
let originRect: DOMRect | null = null;
let originCapturedAt = 0;

if (typeof window !== 'undefined') {
	window.addEventListener(
		'pointerdown',
		(event) => {
			if (!(event.target instanceof Element)) return;
			const rect = event.target.getBoundingClientRect();
			if (rect.width === 0 && rect.height === 0) return;
			originRect = rect;
			originCapturedAt = Date.now();
		},
		{capture: true, passive: true},
	);
}

function takeOriginRect(): DOMRect | null {
	const rect = originRect;
	const capturedAt = originCapturedAt;
	originRect = null;
	originCapturedAt = 0;
	if (!rect || Date.now() - capturedAt > ORIGIN_MAX_AGE_MS) return null;
	return rect;
}

const DOWNLOADS_DESCRIPTOR = msg({
	message: 'View downloads',
	comment: 'Button in the app layout native titlebar that opens the downloads panel.',
});
const DOWNLOADS_TITLE_DESCRIPTOR = msg({
	message: 'Downloads',
	comment: 'Heading of the downloads panel.',
});
const CLEAR_ALL_DESCRIPTOR = msg({
	message: 'Clear all',
	comment: 'Clears finished downloads from the downloads panel.',
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
	comment: 'Download state.',
});
const DOWNLOAD_CANCELED_DESCRIPTOR = msg({
	message: 'Canceled, partial file removed',
	comment: 'Download state after the user cancels it.',
});
const DOWNLOAD_FAILED_DESCRIPTOR = msg({
	message: 'Failed',
	comment: 'Download state.',
});
const DOWNLOAD_FAILED_REASON_DESCRIPTOR = msg({
	message: 'Failed: {reason}',
	comment: 'Download state with the failure reason.',
});
const DOWNLOAD_STARTING_DESCRIPTOR = msg({
	message: 'Starting',
	comment: 'Download state before any bytes arrive.',
});
const DOWNLOAD_PROGRESS_DESCRIPTOR = msg({
	message: '{percent}% of {total}',
	comment: 'Download progress with the total size.',
});

const DownloadsIcon: React.FC = () => (
	<svg
		className={styles.icon}
		viewBox={NATIVE_TITLEBAR_DOWNLOADS_ICON_VIEWBOX}
		xmlns="http://www.w3.org/2000/svg"
		data-flx="app.native-downloads.downloads-icon.icon"
	>
		{NATIVE_TITLEBAR_DOWNLOADS_ICON_PATHS.map((path) => (
			<path key={path} d={path} data-flx="app.native-downloads.downloads-icon.path" />
		))}
	</svg>
);

function formatBytes(value: number): string {
	if (value < 1024) return `${value} B`;
	if (value < 1024 * 1024) return `${(value / 1024).toFixed(1)} KB`;
	return `${(value / (1024 * 1024)).toFixed(1)} MB`;
}

function animateParabola(outer: HTMLElement, inner: HTMLElement, toX: number, toY: number, onFinish: () => void): void {
	const rect = outer.getBoundingClientRect();
	const dx = toX - (rect.left + rect.width / 2);
	const dy = toY - (rect.top + rect.height / 2);
	const horizontal = outer.animate([{transform: 'translateX(0px)'}, {transform: `translateX(${dx}px)`}], {
		duration: ARC_DURATION_MS,
		easing: 'linear',
		fill: 'forwards',
	});
	const vertical = inner.animate(
		[{transform: 'translateY(0px) rotate(30deg)'}, {transform: `translateY(${dy}px) rotate(0deg)`}],
		{
			duration: ARC_DURATION_MS,
			easing: 'cubic-bezier(0.55, 0.1, 0.65, 0.5)',
			fill: 'forwards',
		},
	);
	void Promise.all([horizontal.finished, vertical.finished]).then(
		() => onFinish(),
		() => onFinish(),
	);
}

export const NativeDownloads: React.FC = observer(() => {
	const {i18n} = useLingui();
	const displayDownload = (entry: DownloadEntryEvent): string => {
		switch (entry.state) {
			case 'completed':
				return i18n._(DOWNLOAD_COMPLETED_DESCRIPTOR);
			case 'canceled':
				return i18n._(DOWNLOAD_CANCELED_DESCRIPTOR);
			case 'failed': {
				const reason = entry.error;
				return reason ? i18n._(DOWNLOAD_FAILED_REASON_DESCRIPTOR, {reason}) : i18n._(DOWNLOAD_FAILED_DESCRIPTOR);
			}
			case 'started':
				return i18n._(DOWNLOAD_STARTING_DESCRIPTOR);
			default:
				if (entry.totalBytes != null && entry.totalBytes > 0) {
					return i18n._(DOWNLOAD_PROGRESS_DESCRIPTOR, {
						percent: Math.round((entry.receivedBytes / entry.totalBytes) * 100),
						total: formatBytes(entry.totalBytes),
					});
				}
				return formatBytes(entry.receivedBytes);
		}
	};
	const [arc, setArc] = useState<{x: number; y: number} | null>(null);
	const [landingCount, setLandingCount] = useState(0);
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const arcRef = useRef<HTMLDivElement | null>(null);
	const arcIconRef = useRef<HTMLDivElement | null>(null);
	const arcEntryId = useRef<string | null>(null);
	useEffect(() => {
		if (!DownloadsStore.hasEntries) {
			setLandingCount(0);
		}
	}, [DownloadsStore.hasEntries]);
	useLayoutEffect(() => {
		const entry = DownloadsStore.entries[0];
		if (arc || !entry || arcEntryId.current === entry.id) return;
		if (entry.state !== 'started' && entry.state !== 'progressing') return;
		arcEntryId.current = entry.id;
		const origin = takeOriginRect();
		if (origin) setArc({x: origin.right, y: origin.top});
	}, [DownloadsStore.entries, arc]);
	useLayoutEffect(() => {
		if (!arc) return;
		const outer = arcRef.current;
		const inner = arcIconRef.current;
		const target = buttonRef.current?.getBoundingClientRect();
		if (!outer || !inner || !target) {
			setArc(null);
			return;
		}
		animateParabola(outer, inner, target.left + target.width / 2, target.top + target.height / 2, () => {
			setArc(null);
			setLandingCount((previous) => previous + 1);
			if (landingCount === 0) {
				ComponentBus.dispatch('DOWNLOADS_POPOUT_OPEN');
			}
		});
	}, [arc, landingCount]);
	const {openProps} = usePopout(DOWNLOADS_POPOUT_ID);
	if (!DownloadsStore.hasEntries) {
		return null;
	}
	const isAwaitingLanding = landingCount === 0 && arc !== null;
	return (
		<>
			<Popout
				data-flx="app.native-downloads.popout"
				{...openProps}
				position="bottom-end"
				offsetMainAxis={1}
				ignoreTitlebarBoundary
				subscribeTo="DOWNLOADS_POPOUT_OPEN"
				render={() => (
					<div className={styles.panel} data-flx="app.native-downloads.panel">
						<div className={styles.header} data-flx="app.native-downloads.header">
							<span className={styles.title} data-flx="app.native-downloads.title">
								{i18n._(DOWNLOADS_TITLE_DESCRIPTOR)}
							</span>
							<button
								type="button"
								className={styles.clearAll}
								onClick={DownloadsStore.clearAll}
								data-flx="app.native-downloads.clear-all.button"
							>
								{i18n._(CLEAR_ALL_DESCRIPTOR)}
							</button>
						</div>
						<ul className={styles.list} data-flx="app.native-downloads.list">
							{DownloadsStore.entries.map((entry) => {
								const isDownloading = entry.state === 'started' || entry.state === 'progressing';
								return (
									<li key={entry.id} className={styles.item} data-flx="app.native-downloads.item">
										<button
											type="button"
											className={styles.itemButton}
											onClick={() => DownloadsStore.open(entry)}
											disabled={entry.state !== 'completed'}
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
										<div className={styles.itemControls} data-flx="app.native-downloads.item-controls">
											<button
												key={entry.state === 'completed' ? 'reveal-ready' : 'reveal-pending'}
												type="button"
												className={clsx(styles.cancelOrDismiss, entry.state === 'completed' && styles.swapIcon)}
												onClick={() => DownloadsStore.reveal(entry)}
												aria-label={i18n._(SHOW_IN_FOLDER_DESCRIPTOR)}
												data-flx="app.native-downloads.cancel-or-dismiss.reveal.button"
											>
												<FolderOpenIcon size={20} weight="bold" data-flx="app.native-downloads.folder-open-icon" />
											</button>
											<button
												key={isDownloading ? 'cancel' : 'dismiss'}
												type="button"
												className={clsx(
													styles.cancelOrDismiss,
													styles.swapIcon,
													isDownloading && styles.cancelOrDismissDanger,
												)}
												onClick={() => (isDownloading ? DownloadsStore.cancel(entry) : DownloadsStore.dismiss(entry))}
												aria-label={
													isDownloading ? i18n._(CANCEL_DOWNLOAD_DESCRIPTOR) : i18n._(REMOVE_DOWNLOAD_DESCRIPTOR)
												}
												data-flx="app.native-downloads.cancel-or-dismiss.button"
											>
												{isDownloading ? (
													<XIcon size={20} weight="bold" data-flx="app.native-downloads.x-icon" />
												) : (
													<BroomIcon size={20} weight="bold" data-flx="app.native-downloads.broom-icon" />
												)}
											</button>
										</div>
									</li>
								);
							})}
						</ul>
					</div>
				)}
			>
				<FocusRing offset={0} data-flx="app.native-downloads.focus-ring">
					<button
						ref={buttonRef}
						type="button"
						tabIndex={-1}
						className={clsx(NATIVE_TITLEBAR_CLASS.action, styles.trigger, isAwaitingLanding && styles.triggerHidden)}
						aria-label={i18n._(DOWNLOADS_DESCRIPTOR)}
						data-flx="app.native-downloads.trigger.button"
					>
						<DownloadsIcon key={`icon-${landingCount}`} data-flx="app.native-downloads.downloads-icon" />
						{landingCount > 0 && (
							<span key={`splash-${landingCount}`} className={styles.splash} data-flx="app.native-downloads.splash" />
						)}
					</button>
				</FocusRing>
			</Popout>
			{arc &&
				createPortal(
					<div
						ref={arcRef}
						className={styles.arc}
						style={{left: arc.x, top: arc.y}}
						data-flx="app.native-downloads.arc"
					>
						<div ref={arcIconRef} className={styles.arcIcon} data-flx="app.native-downloads.arc-icon">
							<DownloadSimpleIcon weight="bold" data-flx="app.native-downloads.download-simple-icon" />
						</div>
					</div>,
					document.body,
				)}
		</>
	);
});
