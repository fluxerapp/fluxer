// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import Channels from '@app/features/channel/state/Channels';
import {CANCEL_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Spinner} from '@app/features/ui/components/Spinner';
import {type TabItem, Tabs} from '@app/features/ui/tabs/Tabs';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import {
	loadScreenShareDesktopSourceList,
	loadScreenShareDesktopSources,
} from '@app/features/voice/components/modals/screen_share_picker_modal/DesktopSourceLoader';
import {NativeDisplayPickerState} from '@app/features/voice/components/modals/screen_share_picker_modal/NativeDisplayPickerState';
import {PerWindowAudioNotice} from '@app/features/voice/components/modals/screen_share_picker_modal/PerWindowAudioNotice';
import {PickerEmptyState} from '@app/features/voice/components/modals/screen_share_picker_modal/PickerEmptyState';
import {PickerGrid} from '@app/features/voice/components/modals/screen_share_picker_modal/PickerGrid';
import {ScreenSharePickerDisplayPermissionPrompt} from '@app/features/voice/components/modals/screen_share_picker_modal/ScreenSharePickerDisplayPermissionPrompt';
import {screenRecordingPermissionAllowsPickerSources} from '@app/features/voice/components/modals/screen_share_picker_modal/ScreenSharePickerDisplayPermissionStateMachine';
import {
	DESKTOP_SOURCE_LIST_POLL_INTERVAL_MS,
	desktopSourceIdentitiesMatch,
	getDesktopSourceThumbnailStateKey,
	hasDesktopSourcesMissingThumbnails,
	isDisplaySource,
	isUsableImageDataUrl,
	isWindowSource,
	logger,
	mergeDesktopSources,
	NATIVE_DISPLAY_SELECTION_ID,
	normaliseDesktopSource,
	type PickerCard,
	SCREEN_SHARE_PICKER_TABS,
	type ScreenSharePickerModalProps,
	type ScreenSharePickerPreload,
	type ScreenSharePickerTab,
	THUMBNAIL_REFRESH_DEBOUNCE_MS,
} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';
import {useDeviceEmptyStateCopy} from '@app/features/voice/components/modals/screen_share_picker_modal/useDeviceEmptyStateCopy';
import {useNativePickerCopy} from '@app/features/voice/components/modals/screen_share_picker_modal/useNativePickerCopy';
import {
	readScreenSharePickerScreenRecordingPermission,
	shouldCheckDesktopSourceScreenRecordingPermission,
	useScreenSharePickerDisplayPermission,
} from '@app/features/voice/components/modals/screen_share_picker_modal/useScreenSharePickerDisplayPermission';
import {StreamSettingsMenuContent} from '@app/features/voice/components/StreamSettingsMenuContent';
import MediaEngine, {useVoiceEngineV2Model} from '@app/features/voice/engine/MediaEngineFacade';
import {selectVoiceEngineV2AppConnection} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import {useMediaDevices} from '@app/features/voice/hooks/useMediaDevices';
import VoiceSettings, {
	type LastScreenShareSource,
	type LastScreenShareSourceKind,
} from '@app/features/voice/state/VoiceSettings';
import {getNativeAudioAvailabilityCached} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {
	getDisplayShareEnvironment,
	shouldShowDesktopDownloadCta,
	supportsDeviceScreenShare,
	usesNativeDisplaySharePicker,
} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	normaliseDeviceScreenShareSettings,
	startConfiguredDeviceScreenShare,
	startConfiguredDisplayScreenShare,
	switchConfiguredDeviceScreenShare,
	switchConfiguredDisplayScreenShare,
} from '@app/features/voice/utils/ScreenShareStartFlow';
import {formatFallbackCameraLabel} from '@app/features/voice/utils/VoiceMessageDescriptors';
import type {DesktopSource, NativeAudioAvailability} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AppWindowIcon, GearIcon, InfoIcon, MonitorIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {
	type MouseEvent as ReactMouseEvent,
	type ReactNode,
	useCallback,
	useEffect,
	useMemo,
	useRef,
	useState,
} from 'react';

export type {ScreenSharePickerTab} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';

const FAILED_TO_LOAD_SHAREABLE_SOURCES_DESCRIPTOR = msg({
	message: 'Failed to load shareable sources.',
	comment: 'Error text shown in the screen-share picker when the list of shareable windows/displays fails to load.',
});
const THIS_PICKER_IS_ONLY_AVAILABLE_IN_THE_DESKTOP_DESCRIPTOR = msg({
	message: 'This picker is only available in the desktop app.',
	comment:
		'Empty state in the screen-share picker shown to web users. Explains that this advanced picker requires the desktop app.',
});
const APP_WINDOW_DESCRIPTOR = msg({
	message: 'App window',
	comment:
		'Fallback label for an app window card in the screen-share picker when the OS does not give us a window title.',
});
const DISPLAY_DESCRIPTOR = msg({
	message: 'Display',
	comment:
		'Fallback label for a display / monitor card in the screen-share picker when the OS does not give us a display name.',
});
const DEFAULT_CAMERA_DESCRIPTOR = msg({
	message: 'Default camera',
	comment: 'Fallback label for the system default camera in the screen-share devices tab.',
});
const APPS_DESCRIPTOR = msg({
	message: 'Apps',
	comment: 'Tab label in the screen-share picker. Lists shareable app windows.',
});
const DISPLAYS_DESCRIPTOR = msg({
	message: 'Displays',
	comment: 'Tab label in the screen-share picker. Lists shareable monitors / displays.',
});
const DEVICES_DESCRIPTOR = msg({
	message: 'Devices',
	comment: 'Tab label in the screen-share picker. Lists cameras and virtual capture devices.',
});
const SWITCH_TO_DEVICE_DESCRIPTOR = msg({
	message: 'Switch to device',
	comment:
		'Primary button in the screen-share picker (switch mode, devices tab). Replaces the current stream with the selected camera/device.',
});
const SWITCH_TO_SOURCE_DESCRIPTOR = msg({
	message: 'Switch to source',
	comment:
		'Primary button in the screen-share picker (switch mode). Replaces the current stream with the selected window or display.',
});
const SHARE_DEVICE_DESCRIPTOR = msg({
	message: 'Share device',
	comment:
		'Primary button in the screen-share picker (start mode, devices tab). Begins sharing the selected camera/device.',
});
const SHARE_SOURCE_DESCRIPTOR = msg({
	message: 'Share source',
	comment: 'Primary button in the screen-share picker (start mode). Begins sharing the selected window or display.',
});
const OPEN_BROWSER_PICKER_DESCRIPTOR = msg({
	message: 'Open browser picker',
	comment:
		"Primary button in the screen-share picker on web. Hands off to the browser's native getDisplayMedia picker.",
});
const OPEN_SYSTEM_PICKER_DESCRIPTOR = msg({
	message: 'Open system picker',
	comment: 'Primary button in the screen-share picker on Linux/Wayland. Hands off to the OS xdg-desktop-portal picker.',
});
const CHANGE_STREAM_SOURCE_DESCRIPTOR = msg({
	message: 'Change stream source',
	comment: 'Modal title for the screen-share picker when switching an ongoing stream to a new source.',
});
const CHOOSE_SOMETHING_TO_SHARE_DESCRIPTOR = msg({
	message: 'Choose something to share',
	comment: 'Modal title for the screen-share picker when starting a new screen share.',
});
const STREAM_SETTINGS_DESCRIPTOR = msg({
	message: 'Stream settings',
	comment: 'Toolbar / menu button label in the screen-share picker. Opens the stream quality settings popover.',
});
const MIRROR_CAMERA_DESCRIPTOR = msg({
	message: 'Mirror camera',
	comment: 'Switch label in the screen-share device picker for flipping shared camera/device video horizontally.',
});
const SCREEN_SHARE_PREVIEWS_ENABLED_DESCRIPTOR = msg({
	message: 'Screen share previews are enabled.',
	comment: 'Compact footer status in the screen-share picker when preview frame uploads are enabled.',
});
const SCREEN_SHARE_PREVIEWS_DISABLED_DESCRIPTOR = msg({
	message: 'Screen share previews are disabled.',
	comment: 'Compact footer status in the screen-share picker when preview frame uploads are disabled.',
});
const DISABLE_SCREEN_SHARE_PREVIEWS_DESCRIPTOR = msg({
	message: 'Disable',
	comment: 'Short footer action in the screen-share picker that disables screen-share preview frame uploads.',
});
const SCREEN_SHARE_PREVIEW_INFO_LABEL_DESCRIPTOR = msg({
	message: 'Learn about screen share previews',
	comment: 'Accessible label for the screen-share picker footer info button.',
});
const SCREEN_SHARE_PREVIEW_INFO_TITLE_DESCRIPTOR = msg({
	message: 'Screen share previews',
	comment: 'Title of an informational modal explaining screen-share preview frame uploads.',
});
const SCREEN_SHARE_PREVIEW_INFO_BODY_GUILD_DESCRIPTOR = msg({
	message:
		'When previews are enabled, {productName} uploads occasional JPEG frames from your screen share so people with permission to connect to this voice channel can see a thumbnail before they watch.',
	comment:
		'Explanation of screen-share preview uploads in a guild voice channel. productName is the app name; JPEG is an image format name.',
});
const SCREEN_SHARE_PREVIEW_INFO_BODY_GROUP_DM_DESCRIPTOR = msg({
	message:
		'When previews are enabled, {productName} uploads occasional JPEG frames from your screen share so other people in this group DM can see a thumbnail before they watch.',
	comment:
		'Explanation of screen-share preview uploads in a group DM call. productName is the app name; JPEG is an image format name.',
});
const SCREEN_SHARE_PREVIEW_INFO_BODY_DM_DESCRIPTOR = msg({
	message:
		'When previews are enabled, {productName} uploads occasional JPEG frames from your screen share so the other person in this DM can see a thumbnail before they watch.',
	comment:
		'Explanation of screen-share preview uploads in a one-to-one DM call. productName is the app name; JPEG is an image format name.',
});
const SCREEN_SHARE_PREVIEW_PRIVACY_BODY_GUILD_DESCRIPTOR = msg({
	message:
		'Preview images are stored by {productName} temporarily for delivery and are not end-to-end encrypted. People still need permission to connect, but they do not need to be actively watching your stream.',
	comment:
		'Privacy explanation for screen-share previews in a guild voice channel. productName is the app name. Refers to preview thumbnail images generated from a screen share.',
});
const SCREEN_SHARE_PREVIEW_PRIVACY_BODY_GROUP_DM_DESCRIPTOR = msg({
	message:
		'Preview images are stored by {productName} temporarily for delivery and are not end-to-end encrypted. People in this group DM do not need to be actively watching your stream to see the thumbnail.',
	comment:
		'Privacy explanation for screen-share previews in a group DM call. productName is the app name. Refers to preview thumbnail images generated from a screen share.',
});
const SCREEN_SHARE_PREVIEW_PRIVACY_BODY_DM_DESCRIPTOR = msg({
	message:
		'Preview images are stored by {productName} temporarily for delivery and are not end-to-end encrypted. Disable previews if you do not want screen-share frames uploaded for thumbnails.',
	comment:
		'Privacy explanation for screen-share previews in a one-to-one DM call. productName is the app name. Refers to preview thumbnail images generated from a screen share.',
});
const SCREEN_SHARE_PREVIEW_DISABLE_BODY_DESCRIPTOR = msg({
	message:
		'Disabling previews stops future preview uploads. Existing preview images may remain visible until the stream ends or the preview refreshes.',
	comment:
		'Explains what happens after disabling screen-share preview uploads. Refers to already-uploaded preview thumbnails.',
});
const SCREEN_SHARE_PREVIEW_TOGGLE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Upload preview frames for screen shares.',
	comment: 'Description for the screen-share preview toggle in the informational modal.',
});

const SCREEN_SHARE_PICKER_PRELOAD_CACHE_MS = 1500;
const LAST_SCREEN_SHARE_SOURCE_MAX_AGE_MS = 30 * 24 * 60 * 60 * 1000;

let screenSharePickerPreloadCache: {
	expiresAt: number;
	promise: Promise<ScreenSharePickerPreload>;
} | null = null;

function recordLastScreenShareSource(kind: LastScreenShareSourceKind, sourceId: string | null, title: string): void {
	VoiceSettings.setLastScreenShareSource({
		kind,
		sourceId,
		title,
		updatedAt: Date.now(),
	});
}

function normalizeLastSourceTitle(value: string | undefined): string {
	return (value ?? '')
		.normalize('NFKD')
		.toLowerCase()
		.replace(/[^\p{L}\p{N}]+/gu, ' ')
		.trim();
}

function desktopSourceMatchesLastKind(source: DesktopSource, kind: LastScreenShareSourceKind): boolean {
	if (kind === 'app') return isWindowSource(source);
	if (kind === 'display') return isDisplaySource(source);
	return false;
}

function findLastDesktopSource(
	lastSource: LastScreenShareSource,
	desktopSources: ReadonlyArray<DesktopSource>,
): DesktopSource | null {
	if (lastSource.kind !== 'app' && lastSource.kind !== 'display') return null;
	const candidates = desktopSources.filter((source) => desktopSourceMatchesLastKind(source, lastSource.kind));
	const lastTitle = normalizeLastSourceTitle(lastSource.title);
	if (lastSource.sourceId) {
		const exactIdMatch = candidates.find((source) => source.id === lastSource.sourceId);
		if (exactIdMatch) {
			if (lastSource.kind === 'display' || normalizeLastSourceTitle(exactIdMatch.name) === lastTitle) {
				return exactIdMatch;
			}
		}
	}
	if (!lastTitle) return null;
	const titleMatches = candidates.filter((source) => normalizeLastSourceTitle(source.name) === lastTitle);
	return titleMatches.length === 1 ? titleMatches[0] : null;
}

function getDesktopSourceDimensions(source: DesktopSource): {width: number; height: number} | undefined {
	return source.nativeWidth && source.nativeHeight
		? {width: source.nativeWidth, height: source.nativeHeight}
		: undefined;
}

async function tryStartLastDesktopScreenShareSource(lastSource: LastScreenShareSource): Promise<boolean> {
	const preload = await preloadScreenSharePickerSources();
	if (usesNativeDisplaySharePicker(preload.displayShareEnvironment)) return false;
	const desktopSources = preload.desktopSources.map(normaliseDesktopSource);
	const source = findLastDesktopSource(lastSource, desktopSources);
	if (!source) return false;
	const preferredDisplaySurface = lastSource.kind === 'app' ? 'window' : 'monitor';
	const didStart = await startConfiguredDisplayScreenShare(source.id, {
		sourceDimensions: getDesktopSourceDimensions(source),
		preferredDisplaySurface,
		isOwnWindow: source.isOwnWindow === true,
	});
	if (didStart) {
		recordLastScreenShareSource(lastSource.kind, source.id, source.name || lastSource.title);
	}
	return didStart;
}

async function tryStartLastDeviceScreenShareSource(lastSource: LastScreenShareSource): Promise<boolean> {
	if (!lastSource.sourceId) return false;
	const didStart = await startConfiguredDeviceScreenShare(lastSource.sourceId);
	if (didStart) {
		recordLastScreenShareSource('device', lastSource.sourceId, lastSource.title);
	}
	return didStart;
}

export async function tryStartLastScreenShareSource(): Promise<boolean> {
	const lastSource = VoiceSettings.getLastScreenShareSource();
	if (!lastSource) return false;
	if (Date.now() - lastSource.updatedAt > LAST_SCREEN_SHARE_SOURCE_MAX_AGE_MS) return false;
	try {
		if (lastSource.kind === 'device') {
			return await tryStartLastDeviceScreenShareSource(lastSource);
		}
		return await tryStartLastDesktopScreenShareSource(lastSource);
	} catch (error) {
		logger.warn('Failed to start last screen-share source', {error, kind: lastSource.kind});
		return false;
	}
}

async function loadScreenSharePickerPreload(): Promise<ScreenSharePickerPreload> {
	const displayShareEnvironment = await getDisplayShareEnvironment();
	if (displayShareEnvironment === 'desktop-wayland') {
		return {desktopSources: [], displayShareEnvironment};
	}
	if (usesNativeDisplaySharePicker(displayShareEnvironment)) {
		return {desktopSources: [], displayShareEnvironment};
	}
	if (shouldCheckDesktopSourceScreenRecordingPermission(displayShareEnvironment)) {
		const screenRecordingPermission = await readScreenSharePickerScreenRecordingPermission('preload');
		if (!screenRecordingPermissionAllowsPickerSources(screenRecordingPermission)) {
			return {desktopSources: [], desktopSourcesSkippedForPermission: true, displayShareEnvironment};
		}
	}
	const desktopSourcesPromise = getElectronAPI()
		? loadScreenShareDesktopSources().catch((error) => {
				logger.warn('Failed to preload desktop sources for picker', {error});
				return [];
			})
		: Promise.resolve([]);
	const desktopSources = await desktopSourcesPromise;
	return {desktopSources, displayShareEnvironment};
}

export async function preloadScreenSharePickerSources(): Promise<ScreenSharePickerPreload> {
	const now = Date.now();
	if (screenSharePickerPreloadCache && screenSharePickerPreloadCache.expiresAt > now) {
		return screenSharePickerPreloadCache.promise;
	}
	const promise = loadScreenSharePickerPreload();
	screenSharePickerPreloadCache = {
		expiresAt: now + SCREEN_SHARE_PICKER_PRELOAD_CACHE_MS,
		promise,
	};
	return promise;
}

export async function openScreenSharePickerModal(): Promise<void> {
	ModalCommands.push(
		ModalCommands.modal(() => (
			<ScreenSharePickerModalPreloader data-flx="voice.screen-share-picker-modal.open-screen-share-picker-modal.preloader" />
		)),
	);
}

export async function openScreenShareSourceSwitcherModal(
	options: {initialTab?: ScreenSharePickerTab} = {},
): Promise<void> {
	ModalCommands.push(
		ModalCommands.modal(() => (
			<ScreenSharePickerModalPreloader
				initialTab={options.initialTab}
				mode="switch"
				data-flx="voice.screen-share-picker-modal.open-screen-share-source-switcher-modal.preloader"
			/>
		)),
	);
}

type ScreenSharePickerMode = 'start' | 'switch';
type ScreenSharePreviewCallContext = 'guild' | 'group_dm' | 'dm';

function getScreenSharePreviewCallContext(channelId: string | null): ScreenSharePreviewCallContext {
	const channel = channelId ? Channels.getChannel(channelId) : undefined;
	if (channel?.isDM()) return 'dm';
	if (channel?.isGroupDM()) return 'group_dm';
	return 'guild';
}

function getScreenSharePreviewInfoBodyDescriptor(context: ScreenSharePreviewCallContext) {
	if (context === 'dm') return SCREEN_SHARE_PREVIEW_INFO_BODY_DM_DESCRIPTOR;
	if (context === 'group_dm') return SCREEN_SHARE_PREVIEW_INFO_BODY_GROUP_DM_DESCRIPTOR;
	return SCREEN_SHARE_PREVIEW_INFO_BODY_GUILD_DESCRIPTOR;
}

function getScreenSharePreviewPrivacyBodyDescriptor(context: ScreenSharePreviewCallContext) {
	if (context === 'dm') return SCREEN_SHARE_PREVIEW_PRIVACY_BODY_DM_DESCRIPTOR;
	if (context === 'group_dm') return SCREEN_SHARE_PREVIEW_PRIVACY_BODY_GROUP_DM_DESCRIPTOR;
	return SCREEN_SHARE_PREVIEW_PRIVACY_BODY_GUILD_DESCRIPTOR;
}

function clampScreenSharePickerTab(tab: ScreenSharePickerTab | undefined): ScreenSharePickerTab {
	if (tab === 'devices' && !supportsDeviceScreenShare()) return 'apps';
	return tab ?? 'apps';
}

interface ScreenSharePickerModalFrameProps {
	activeTab: ScreenSharePickerTab;
	children: ReactNode;
	dataFlxPrefix: string;
	mode: ScreenSharePickerMode;
	onActiveTabChange: (tab: ScreenSharePickerTab) => void;
}

function ScreenSharePickerModalFrame({
	activeTab,
	children,
	dataFlxPrefix,
	mode,
	onActiveTabChange,
}: ScreenSharePickerModalFrameProps) {
	const {i18n} = useLingui();
	const tabs = useMemo<Array<TabItem<ScreenSharePickerTab>>>(() => {
		const items: Array<TabItem<ScreenSharePickerTab>> = [
			{key: 'apps', label: i18n._(APPS_DESCRIPTOR)},
			{key: 'displays', label: i18n._(DISPLAYS_DESCRIPTOR)},
		];
		if (supportsDeviceScreenShare()) {
			items.push({key: 'devices', label: i18n._(DEVICES_DESCRIPTOR)});
		}
		return items;
	}, [i18n.locale]);
	return (
		<Modal.Root
			size="large"
			centered
			onClose={ModalCommands.pop}
			className={styles.root}
			data-flx={`${dataFlxPrefix}.root`}
		>
			<Modal.ScreenReaderLabel
				text={
					mode === 'switch' ? i18n._(CHANGE_STREAM_SOURCE_DESCRIPTOR) : i18n._(CHOOSE_SOMETHING_TO_SHARE_DESCRIPTOR)
				}
				data-flx={`${dataFlxPrefix}.modal-screen-reader-label`}
			/>
			<div className={styles.topBar} data-flx={`${dataFlxPrefix}.top-bar`}>
				<Tabs
					tabs={tabs}
					activeTab={activeTab}
					onTabChange={onActiveTabChange}
					className={styles.tabs}
					data-flx={`${dataFlxPrefix}.tabs`}
				/>
			</div>
			{children}
		</Modal.Root>
	);
}

const ScreenSharePreviewInfoModal = observer(() => {
	const {i18n} = useLingui();
	const voiceModel = useVoiceEngineV2Model();
	const callContext = getScreenSharePreviewCallContext(
		selectVoiceEngineV2AppConnection(voiceModel).channelId ?? MediaEngine.channelId,
	);
	const previewsEnabled = !PrivacyPreferences.getDisableStreamPreviews();
	const handlePreviewToggle = useCallback((enabled: boolean) => {
		PrivacyPreferences.setDisableStreamPreviews(!enabled);
	}, []);
	return (
		<Modal.Root
			size="small"
			centered
			onClose={ModalCommands.pop}
			data-flx="voice.screen-share-picker-modal.preview-info-modal.root"
		>
			<Modal.Header
				title={i18n._(SCREEN_SHARE_PREVIEW_INFO_TITLE_DESCRIPTOR)}
				data-flx="voice.screen-share-picker-modal.preview-info-modal.header"
			/>
			<Modal.Content data-flx="voice.screen-share-picker-modal.preview-info-modal.content">
				<Modal.ContentLayout
					className={styles.previewInfoContent}
					data-flx="voice.screen-share-picker-modal.preview-info-modal.content-layout"
				>
					<Modal.Description data-flx="voice.screen-share-picker-modal.preview-info-modal.description">
						{i18n._(getScreenSharePreviewInfoBodyDescriptor(callContext), {productName: PRODUCT_NAME})}
					</Modal.Description>
					<p className={styles.previewInfoParagraph} data-flx="voice.screen-share-picker-modal.preview-info-modal.e2ee">
						{i18n._(getScreenSharePreviewPrivacyBodyDescriptor(callContext), {productName: PRODUCT_NAME})}
					</p>
					<p
						className={styles.previewInfoParagraph}
						data-flx="voice.screen-share-picker-modal.preview-info-modal.disable"
					>
						{i18n._(SCREEN_SHARE_PREVIEW_DISABLE_BODY_DESCRIPTOR)}
					</p>
					<Switch
						className={styles.previewInfoSwitch}
						label={i18n._(SCREEN_SHARE_PREVIEW_INFO_TITLE_DESCRIPTOR)}
						description={i18n._(SCREEN_SHARE_PREVIEW_TOGGLE_DESCRIPTION_DESCRIPTOR)}
						value={previewsEnabled}
						onChange={handlePreviewToggle}
						data-flx="voice.screen-share-picker-modal.preview-info-modal.switch"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

const ScreenSharePreviewFooterNotice = observer(() => {
	const {i18n} = useLingui();
	const previewsEnabled = !PrivacyPreferences.getDisableStreamPreviews();
	const openInfoModal = useCallback(() => {
		ModalCommands.push(
			ModalCommands.modal(() => (
				<ScreenSharePreviewInfoModal data-flx="voice.screen-share-picker-modal.open-info-modal.screen-share-preview-info-modal" />
			)),
		);
	}, []);
	const handleDisable = useCallback(() => {
		PrivacyPreferences.setDisableStreamPreviews(true);
	}, []);
	return (
		<div className={styles.previewNotice} data-flx="voice.screen-share-picker-modal.preview-footer-notice">
			<span className={styles.previewNoticeText} data-flx="voice.screen-share-picker-modal.preview-footer-notice.text">
				{i18n._(previewsEnabled ? SCREEN_SHARE_PREVIEWS_ENABLED_DESCRIPTOR : SCREEN_SHARE_PREVIEWS_DISABLED_DESCRIPTOR)}
			</span>
			{previewsEnabled && (
				<Button
					variant="secondary"
					small
					compact
					onClick={handleDisable}
					data-flx="voice.screen-share-picker-modal.preview-footer-notice.button.disable"
				>
					{i18n._(DISABLE_SCREEN_SHARE_PREVIEWS_DESCRIPTOR)}
				</Button>
			)}
			<Button
				variant="secondary"
				square
				small
				compact
				className={styles.previewNoticeInfoButton}
				icon={
					<InfoIcon
						size={16}
						weight="fill"
						data-flx="voice.screen-share-picker-modal.preview-footer-notice.info-icon"
					/>
				}
				aria-label={i18n._(SCREEN_SHARE_PREVIEW_INFO_LABEL_DESCRIPTOR)}
				onClick={openInfoModal}
				data-flx="voice.screen-share-picker-modal.preview-footer-notice.button.info"
			/>
		</div>
	);
});

const ScreenSharePickerModalPreloader = observer(
	({initialTab, mode = 'start'}: {initialTab?: ScreenSharePickerTab; mode?: ScreenSharePickerMode}) => {
		const {i18n} = useLingui();
		const [activeTab, setActiveTab] = useState<ScreenSharePickerTab>(() => clampScreenSharePickerTab(initialTab));
		const [preload, setPreload] = useState<ScreenSharePickerPreload | null>(null);
		const [loadError, setLoadError] = useState<string | null>(null);
		const mountedRef = useRef(true);
		useEffect(
			() => () => {
				mountedRef.current = false;
			},
			[],
		);
		const loadPreload = useCallback(async () => {
			setLoadError(null);
			try {
				const nextPreload = await preloadScreenSharePickerSources();
				if (!mountedRef.current) return;
				setPreload(nextPreload);
			} catch (error) {
				logger.error('Failed to preload screen share picker sources', {error});
				if (!mountedRef.current) return;
				setLoadError(i18n._(FAILED_TO_LOAD_SHAREABLE_SOURCES_DESCRIPTOR));
			}
		}, [i18n]);
		useEffect(() => {
			void loadPreload();
		}, [loadPreload]);
		return (
			<ScreenSharePickerModalFrame
				activeTab={activeTab}
				dataFlxPrefix="voice.screen-share-picker-modal.preloader"
				mode={mode}
				onActiveTabChange={setActiveTab}
				data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-preloader.screen-share-picker-modal-frame"
			>
				{preload ? (
					<ScreenSharePickerModalLoadedContent
						activeTab={activeTab}
						displayShareEnvironment={preload.displayShareEnvironment}
						initialDesktopSources={preload.desktopSources}
						initialDesktopSourcesSkippedForPermission={preload.desktopSourcesSkippedForPermission}
						mode={mode}
						onActiveTabChange={setActiveTab}
						data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-preloader.screen-share-picker-modal-loaded-content"
					/>
				) : (
					<>
						<Modal.Content
							padding="none"
							className={styles.content}
							showTrack={false}
							data-flx="voice.screen-share-picker-modal.preloader.content"
						>
							{loadError ? (
								<div className={styles.state} data-flx="voice.screen-share-picker-modal.preloader.state">
									<div className={styles.stateTitle} data-flx="voice.screen-share-picker-modal.preloader.state-title">
										{loadError}
									</div>
									<Button
										variant="secondary"
										onClick={() => void loadPreload()}
										data-flx="voice.screen-share-picker-modal.preloader.button.retry"
									>
										{i18n._(TRY_AGAIN_DESCRIPTOR)}
									</Button>
								</div>
							) : (
								<div className={styles.loadingState} data-flx="voice.screen-share-picker-modal.preloader.loading-state">
									<Spinner size="large" data-flx="voice.screen-share-picker-modal.preloader.spinner" />
								</div>
							)}
						</Modal.Content>
						<Modal.Footer className={styles.footer} data-flx="voice.screen-share-picker-modal.preloader.footer">
							<div className={styles.footerStart} data-flx="voice.screen-share-picker-modal.preloader.footer-start">
								<ScreenSharePreviewFooterNotice data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-preloader.screen-share-preview-footer-notice" />
							</div>
							<Button
								variant="secondary"
								onClick={() => ModalCommands.pop()}
								data-flx="voice.screen-share-picker-modal.preloader.button.pop"
							>
								{i18n._(CANCEL_DESCRIPTOR)}
							</Button>
						</Modal.Footer>
					</>
				)}
			</ScreenSharePickerModalFrame>
		);
	},
);

interface ScreenSharePickerModalLoadedContentProps extends ScreenSharePickerModalProps {
	activeTab: ScreenSharePickerTab;
	onActiveTabChange: (tab: ScreenSharePickerTab) => void;
}

const ScreenSharePickerModalLoadedContent = observer(
	({
		initialDesktopSources,
		initialDesktopSourcesSkippedForPermission,
		displayShareEnvironment,
		activeTab,
		onActiveTabChange,
		mode = 'start',
	}: ScreenSharePickerModalLoadedContentProps) => {
		const {i18n} = useLingui();
		const {videoDevices} = useMediaDevices({autoRefresh: activeTab === 'devices', requestPermissions: false});
		const usesNativeDisplayPicker = usesNativeDisplaySharePicker(displayShareEnvironment);
		const showDesktopDownloadCta = shouldShowDesktopDownloadCta(displayShareEnvironment);
		const [desktopSources, setDesktopSources] = useState<Array<DesktopSource>>(
			() => initialDesktopSources?.map(normaliseDesktopSource) ?? [],
		);
		const [hasLoadedDesktopSources, setHasLoadedDesktopSources] = useState(
			(initialDesktopSources != null && initialDesktopSourcesSkippedForPermission !== true) || usesNativeDisplayPicker,
		);
		const [loadError, setLoadError] = useState<string | null>(null);
		const [pendingSelectionId, setPendingSelectionId] = useState<string | null>(null);
		const [invalidThumbnailIds, setInvalidThumbnailIds] = useState<ReadonlySet<string>>(() => new Set());
		const [nativeAudioAvailability, setNativeAudioAvailability] = useState<NativeAudioAvailability | null>(null);
		const loadRequestIdRef = useRef(0);
		const desktopSourcesRef = useRef(desktopSources);
		const desktopSourceRefreshInFlightRef = useRef(false);
		const thumbnailRefreshTimeoutRef = useRef<number | null>(null);
		const thumbnailRecoveryAttemptedForSourceSetRef = useRef<string | null>(null);
		useEffect(() => {
			desktopSourcesRef.current = desktopSources;
		}, [desktopSources]);
		useEffect(() => {
			let cancelled = false;
			void getNativeAudioAvailabilityCached().then((availability) => {
				if (!cancelled) setNativeAudioAvailability(availability);
			});
			return () => {
				cancelled = true;
			};
		}, []);
		const platform = getElectronAPI()?.platform;
		const displayPermission = useScreenSharePickerDisplayPermission({
			activeTab,
			displayShareEnvironment,
		});
		const captureScopeForActiveTab = activeTab === 'apps' ? 'process' : activeTab === 'displays' ? 'system' : null;
		const showPerWindowAudioUnsupportedNotice =
			captureScopeForActiveTab != null &&
			(platform === 'win32' || platform === 'darwin') &&
			nativeAudioAvailability != null &&
			(nativeAudioAvailability.capabilities?.[captureScopeForActiveTab] === false ||
				(!nativeAudioAvailability.available && nativeAudioAvailability.reason === 'os-version-too-old'));
		const loadDesktopSources = useCallback(
			async (options: {force?: boolean; silent?: boolean} = {}) => {
				if (usesNativeDisplayPicker) {
					setHasLoadedDesktopSources(true);
					setDesktopSources([]);
					setLoadError(null);
					return;
				}
				const requestId = ++loadRequestIdRef.current;
				if (!options.silent) {
					setLoadError(null);
				}
				try {
					const nextSources = await loadScreenShareDesktopSources({force: options.force});
					if (requestId !== loadRequestIdRef.current) {
						return;
					}
					if (options.silent && nextSources.length === 0 && desktopSourcesRef.current.length > 0) {
						logger.warn('Ignoring empty silent desktop source refresh while existing sources are available');
						return;
					}
					setDesktopSources((previous) => mergeDesktopSources(previous, nextSources));
					setInvalidThumbnailIds(new Set());
					setHasLoadedDesktopSources(true);
					setLoadError(null);
				} catch (error) {
					logger.error('Failed to load desktop sources for picker', {error});
					if (requestId !== loadRequestIdRef.current) {
						return;
					}
					setHasLoadedDesktopSources(true);
					if (desktopSourcesRef.current.length === 0) {
						setDesktopSources([]);
						setLoadError(
							getElectronAPI()
								? i18n._(FAILED_TO_LOAD_SHAREABLE_SOURCES_DESCRIPTOR)
								: i18n._(THIS_PICKER_IS_ONLY_AVAILABLE_IN_THE_DESKTOP_DESCRIPTOR),
						);
					}
				}
			},
			[usesNativeDisplayPicker, i18n],
		);
		useEffect(() => {
			if (usesNativeDisplayPicker || displayPermission.blocksDesktopSources || hasLoadedDesktopSources) {
				return;
			}
			void loadDesktopSources({silent: true});
		}, [displayPermission.blocksDesktopSources, hasLoadedDesktopSources, loadDesktopSources, usesNativeDisplayPicker]);
		useEffect(() => {
			if (usesNativeDisplayPicker || displayPermission.blocksDesktopSources || activeTab === 'devices') {
				return;
			}
			let cancelled = false;
			const tick = async (): Promise<void> => {
				if (cancelled || desktopSourceRefreshInFlightRef.current) return;
				if (document.visibilityState === 'hidden' || pendingSelectionId) return;
				desktopSourceRefreshInFlightRef.current = true;
				try {
					const nextList = await loadScreenShareDesktopSourceList();
					if (cancelled) return;
					const current = desktopSourcesRef.current;
					if (desktopSourceIdentitiesMatch(current, nextList)) {
						return;
					}
					const currentIds = new Set(current.map((source) => source.id));
					const hasNewSources = nextList.some((source) => !currentIds.has(source.id));
					if (hasNewSources) {
						await loadDesktopSources({force: true, silent: true});
						return;
					}
					setDesktopSources((previous) => mergeDesktopSources(previous, nextList));
					const nextIds = new Set(nextList.map((source) => source.id));
					setInvalidThumbnailIds((previous) => {
						let changed = false;
						const next = new Set<string>();
						for (const id of previous) {
							if (nextIds.has(id)) {
								next.add(id);
							} else {
								changed = true;
							}
						}
						return changed ? next : previous;
					});
				} catch (error) {
					logger.warn('Desktop source list poll failed', {error});
				} finally {
					desktopSourceRefreshInFlightRef.current = false;
				}
			};
			const interval = window.setInterval(() => {
				void tick();
			}, DESKTOP_SOURCE_LIST_POLL_INTERVAL_MS);
			return () => {
				cancelled = true;
				window.clearInterval(interval);
			};
		}, [
			activeTab,
			displayPermission.blocksDesktopSources,
			loadDesktopSources,
			pendingSelectionId,
			usesNativeDisplayPicker,
		]);
		useEffect(
			() => () => {
				if (thumbnailRefreshTimeoutRef.current != null) {
					window.clearTimeout(thumbnailRefreshTimeoutRef.current);
				}
			},
			[],
		);
		const activeDesktopSourceThumbnailStateKey = useMemo(() => {
			if (activeTab === 'devices') {
				return null;
			}
			const predicate = activeTab === 'apps' ? isWindowSource : isDisplaySource;
			return `${activeTab}:${getDesktopSourceThumbnailStateKey(desktopSources, predicate)}`;
		}, [activeTab, desktopSources]);
		const activeDesktopSourcesMissingThumbnails = useMemo(() => {
			if (activeTab === 'devices') {
				return false;
			}
			const predicate = activeTab === 'apps' ? isWindowSource : isDisplaySource;
			return hasDesktopSourcesMissingThumbnails(desktopSources, predicate);
		}, [activeTab, desktopSources]);
		useEffect(() => {
			if (
				usesNativeDisplayPicker ||
				displayPermission.blocksDesktopSources ||
				activeTab === 'devices' ||
				!hasLoadedDesktopSources ||
				pendingSelectionId ||
				!activeDesktopSourcesMissingThumbnails ||
				!activeDesktopSourceThumbnailStateKey
			) {
				return;
			}
			if (thumbnailRecoveryAttemptedForSourceSetRef.current === activeDesktopSourceThumbnailStateKey) {
				return;
			}
			thumbnailRecoveryAttemptedForSourceSetRef.current = activeDesktopSourceThumbnailStateKey;
			logger.debug('Refreshing desktop sources because picker thumbnails are missing', {activeTab});
			void loadDesktopSources({force: true, silent: true});
		}, [
			activeDesktopSourceThumbnailStateKey,
			activeDesktopSourcesMissingThumbnails,
			activeTab,
			displayPermission.blocksDesktopSources,
			hasLoadedDesktopSources,
			loadDesktopSources,
			pendingSelectionId,
			usesNativeDisplayPicker,
		]);
		const appCards = useMemo<Array<PickerCard>>(() => {
			return desktopSources.filter(isWindowSource).map((source) => ({
				id: source.id,
				title: source.name || i18n._(APP_WINDOW_DESCRIPTOR),
				thumbnailSrc:
					!invalidThumbnailIds.has(source.id) && isUsableImageDataUrl(source.thumbnailDataUrl)
						? source.thumbnailDataUrl
						: undefined,
				badgeSrc: isUsableImageDataUrl(source.appIconDataUrl) ? source.appIconDataUrl : undefined,
				placeholderIcon: AppWindowIcon,
			}));
		}, [desktopSources, invalidThumbnailIds, i18n.locale]);
		const displayCards = useMemo<Array<PickerCard>>(() => {
			return desktopSources.filter(isDisplaySource).map((source) => ({
				id: source.id,
				title: source.name || i18n._(DISPLAY_DESCRIPTOR),
				thumbnailSrc:
					!invalidThumbnailIds.has(source.id) && isUsableImageDataUrl(source.thumbnailDataUrl)
						? source.thumbnailDataUrl
						: undefined,
				placeholderIcon: MonitorIcon,
			}));
		}, [desktopSources, invalidThumbnailIds, i18n.locale]);
		const deviceCards = useMemo<Array<PickerCard>>(() => {
			const explicitDevices = videoDevices.filter((device) => device.deviceId && device.deviceId !== 'default');
			const selectableDevices =
				explicitDevices.length > 0 ? explicitDevices : videoDevices.filter((device) => device.deviceId);
			return selectableDevices.map((device) => ({
				id: device.deviceId,
				title:
					device.label ||
					(device.deviceId === 'default' ? i18n._(DEFAULT_CAMERA_DESCRIPTOR) : formatFallbackCameraLabel(i18n)),
				placeholderIcon: VideoCameraIcon,
			}));
		}, [videoDevices, i18n.locale]);
		const tabCards = useMemo<Record<ScreenSharePickerTab, Array<PickerCard>>>(
			() => ({
				apps: appCards,
				displays: displayCards,
				devices: deviceCards,
			}),
			[appCards, deviceCards, displayCards],
		);
		useEffect(() => {
			if (activeTab !== 'devices') {
				return;
			}
			normaliseDeviceScreenShareSettings();
		}, [activeTab]);
		useEffect(() => {
			if (activeTab === 'devices') {
				return;
			}
			if (displayPermission.blocksDesktopSources) {
				return;
			}
			if (usesNativeDisplayPicker) {
				return;
			}
			if (tabCards[activeTab].length > 0 || !hasLoadedDesktopSources) {
				return;
			}
			const fallbackTab = SCREEN_SHARE_PICKER_TABS.find((tab) => tabCards[tab].length > 0);
			if (fallbackTab && fallbackTab !== activeTab) {
				onActiveTabChange(fallbackTab);
			}
		}, [
			activeTab,
			displayPermission.blocksDesktopSources,
			hasLoadedDesktopSources,
			onActiveTabChange,
			tabCards,
			usesNativeDisplayPicker,
		]);
		const handleStartSelection = useCallback(
			async (cardId: string) => {
				if (pendingSelectionId) {
					return;
				}
				setPendingSelectionId(cardId);
				try {
					const selectedSource = desktopSourcesRef.current.find((source) => source.id === cardId);
					const sourceDimensions =
						selectedSource?.nativeWidth && selectedSource.nativeHeight
							? {width: selectedSource.nativeWidth, height: selectedSource.nativeHeight}
							: undefined;
					let didSelect: boolean;
					if (activeTab === 'devices') {
						didSelect =
							mode === 'switch'
								? await switchConfiguredDeviceScreenShare(cardId)
								: await startConfiguredDeviceScreenShare(cardId);
					} else {
						const selectedDisplaySourceId = usesNativeDisplayPicker ? null : cardId;
						const preferredDisplaySurface: 'window' | 'monitor' | undefined =
							activeTab === 'apps' ? 'window' : activeTab === 'displays' ? 'monitor' : undefined;
						const isOwnWindow = selectedSource?.isOwnWindow === true;
						didSelect =
							mode === 'switch'
								? await switchConfiguredDisplayScreenShare(selectedDisplaySourceId, {
										sourceDimensions,
										preferredDisplaySurface,
										isOwnWindow,
									})
								: await startConfiguredDisplayScreenShare(selectedDisplaySourceId, {
										sourceDimensions,
										preferredDisplaySurface,
										isOwnWindow,
									});
					}
					if (didSelect) {
						const selectedCard = tabCards[activeTab].find((card) => card.id === cardId);
						const kind: LastScreenShareSourceKind =
							activeTab === 'devices' ? 'device' : activeTab === 'apps' ? 'app' : 'display';
						recordLastScreenShareSource(
							kind,
							activeTab === 'devices' ? cardId : (selectedSource?.id ?? cardId),
							selectedCard?.title ?? selectedSource?.name ?? cardId,
						);
						ModalCommands.pop();
					} else if (activeTab !== 'devices') {
						void loadDesktopSources({force: true, silent: true});
					}
				} catch (error) {
					logger.warn('Screen share selection failed; invalidating source cache', {error, cardId});
					if (activeTab !== 'devices') {
						void loadDesktopSources({force: true, silent: true});
					}
					throw error;
				} finally {
					setPendingSelectionId(null);
				}
			},
			[activeTab, loadDesktopSources, mode, pendingSelectionId, platform, tabCards, usesNativeDisplayPicker],
		);
		const handleSettingsClick = useCallback(
			(event: ReactMouseEvent<HTMLButtonElement>) => {
				ContextMenuCommands.openAboveElementBottomRight(event, () => (
					<StreamSettingsMenuContent
						applyToLiveStream={false}
						shareContext={activeTab === 'devices' ? 'device' : activeTab === 'apps' ? 'app' : 'display'}
						displayShareEnvironment={displayShareEnvironment}
						data-flx="voice.screen-share-picker-modal.handle-settings-click.stream-settings-menu-content"
					/>
				));
			},
			[activeTab, displayShareEnvironment],
		);
		const handlePreviewImageError = useCallback(
			(cardId: string) => {
				setInvalidThumbnailIds((current) => {
					if (current.has(cardId)) {
						return current;
					}
					const next = new Set(current);
					next.add(cardId);
					return next;
				});
				if (thumbnailRefreshTimeoutRef.current != null) {
					window.clearTimeout(thumbnailRefreshTimeoutRef.current);
				}
				thumbnailRefreshTimeoutRef.current = window.setTimeout(() => {
					thumbnailRefreshTimeoutRef.current = null;
					void loadDesktopSources({force: true, silent: true});
				}, THUMBNAIL_REFRESH_DEBOUNCE_MS);
			},
			[loadDesktopSources],
		);
		const activeCards = tabCards[activeTab];
		const showDesktopSourceState = activeTab !== 'devices';
		const showNativeDisplayPickerState = showDesktopSourceState && usesNativeDisplayPicker;
		const activeShareLabel =
			mode === 'switch'
				? activeTab === 'devices'
					? i18n._(SWITCH_TO_DEVICE_DESCRIPTOR)
					: i18n._(SWITCH_TO_SOURCE_DESCRIPTOR)
				: activeTab === 'devices'
					? i18n._(SHARE_DEVICE_DESCRIPTOR)
					: i18n._(SHARE_SOURCE_DESCRIPTOR);
		const showEmptyState = activeTab === 'devices' || hasLoadedDesktopSources || usesNativeDisplayPicker;
		const pickerActionLabel =
			displayShareEnvironment === 'web'
				? i18n._(OPEN_BROWSER_PICKER_DESCRIPTOR)
				: i18n._(OPEN_SYSTEM_PICKER_DESCRIPTOR);
		const nativeDisplayPending = pendingSelectionId === NATIVE_DISPLAY_SELECTION_ID;
		const nativePickerCopy = useNativePickerCopy(activeTab, displayShareEnvironment);
		const deviceEmptyStateCopy = useDeviceEmptyStateCopy(displayShareEnvironment);
		return (
			<>
				<Modal.Content
					padding="none"
					className={styles.content}
					showTrack={false}
					data-flx="voice.screen-share-picker-modal.content"
				>
					{showPerWindowAudioUnsupportedNotice && (
						<PerWindowAudioNotice
							platform={platform}
							mode={captureScopeForActiveTab === 'system' ? 'system' : 'app'}
							data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.per-window-audio-notice"
						/>
					)}
					{showDesktopSourceState && displayPermission.prompt !== 'none' ? (
						<ScreenSharePickerDisplayPermissionPrompt
							prompt={displayPermission.prompt}
							onOpenSettings={displayPermission.openSettings}
							data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.screen-share-picker-display-permission-prompt"
						/>
					) : showDesktopSourceState && !usesNativeDisplayPicker && loadError ? (
						<div className={styles.state} data-flx="voice.screen-share-picker-modal.state">
							<div className={styles.stateTitle} data-flx="voice.screen-share-picker-modal.state-title">
								{loadError}
							</div>
							<Button
								variant="secondary"
								onClick={() => void loadDesktopSources()}
								data-flx="voice.screen-share-picker-modal.button"
							>
								{i18n._(TRY_AGAIN_DESCRIPTOR)}
							</Button>
						</div>
					) : showNativeDisplayPickerState && nativePickerCopy ? (
						<NativeDisplayPickerState
							copy={nativePickerCopy}
							pickerActionLabel={pickerActionLabel}
							onPickerAction={() => void handleStartSelection(NATIVE_DISPLAY_SELECTION_ID)}
							pickerActionPending={nativeDisplayPending}
							showDesktopDownloadCta={showDesktopDownloadCta}
							data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.native-display-picker-state"
						/>
					) : showEmptyState && activeCards.length === 0 ? (
						<PickerEmptyState
							title={deviceEmptyStateCopy.title}
							description={deviceEmptyStateCopy.description}
							data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.picker-empty-state"
						/>
					) : (
						<PickerGrid
							cards={activeCards}
							activeTab={activeTab}
							activeShareLabel={activeShareLabel}
							pendingSelectionId={pendingSelectionId}
							onSelect={(cardId) => void handleStartSelection(cardId)}
							onPreviewImageError={handlePreviewImageError}
							data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.picker-grid"
						/>
					)}
				</Modal.Content>
				<Modal.Footer className={styles.footer} data-flx="voice.screen-share-picker-modal.footer">
					<div className={styles.footerStart} data-flx="voice.screen-share-picker-modal.footer-start">
						<ScreenSharePreviewFooterNotice data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.screen-share-preview-footer-notice" />
						{activeTab === 'devices' && (
							<Switch
								compact
								className={styles.footerMirrorSwitch}
								label={i18n._(MIRROR_CAMERA_DESCRIPTOR)}
								value={VoiceSettings.mirrorCamera}
								onChange={(value) => VoiceSettingsCommands.update({mirrorCamera: value})}
								data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content.footer-mirror-switch.update"
							/>
						)}
					</div>
					<Button
						variant="secondary"
						onClick={() => ModalCommands.pop()}
						data-flx="voice.screen-share-picker-modal.button.pop"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						variant="secondary"
						square
						icon={<GearIcon size={18} weight="fill" data-flx="voice.screen-share-picker-modal.gear-icon" />}
						aria-label={i18n._(STREAM_SETTINGS_DESCRIPTOR)}
						onClick={handleSettingsClick}
						data-flx="voice.screen-share-picker-modal.button.settings-click"
					/>
				</Modal.Footer>
			</>
		);
	},
);
export const ScreenSharePickerModal = observer(function ScreenSharePickerModal({
	initialTab,
	mode = 'start',
	...contentProps
}: ScreenSharePickerModalProps) {
	const [activeTab, setActiveTab] = useState<ScreenSharePickerTab>(() => clampScreenSharePickerTab(initialTab));
	return (
		<ScreenSharePickerModalFrame
			activeTab={activeTab}
			dataFlxPrefix="voice.screen-share-picker-modal"
			mode={mode}
			onActiveTabChange={setActiveTab}
			data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-frame"
		>
			<ScreenSharePickerModalLoadedContent
				data-flx="voice.screen-share-picker-modal.screen-share-picker-modal-loaded-content"
				{...contentProps}
				activeTab={activeTab}
				mode={mode}
				onActiveTabChange={setActiveTab}
			/>
		</ScreenSharePickerModalFrame>
	);
});
