// SPDX-License-Identifier: AGPL-3.0-or-later

import i18nGlobal from '@app/app/I18n';
import {Routes} from '@app/app/Routes';
import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CopyLinkSection} from '@app/features/app/components/dialogs/shared/CopyLinkSection';
import type {RecipientItem} from '@app/features/app/components/dialogs/shared/RecipientList';
import {RecipientList, useRecipientItems} from '@app/features/app/components/dialogs/shared/RecipientList';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {SEARCH_FRIENDS_DESCRIPTOR, SENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import styles from '@app/features/theme/components/modals/ShareThemeModal.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useCopyLinkHandler} from '@app/lib/copy-link';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const CHECK_OUT_MY_CUSTOM_THEME_DESCRIPTOR = msg({
	message: 'Check out my custom theme!',
	comment: 'Description text in the share theme modal.',
});
const SHARE_YOUR_THEME_DESCRIPTOR = msg({
	message: 'Share your theme',
	comment: 'Button or menu action label in the share theme modal. Keep it concise.',
});
const SEND_DESCRIPTOR = msg({
	message: 'Send',
	comment: 'Button or menu action label in the share theme modal. Keep it concise.',
});
const COULDN_T_CREATE_LINK_TITLE_DESCRIPTOR = msg({
	message: "Couldn't create a theme link",
	comment: 'Title of the generic fallback error modal shown when generating a shareable theme link fails.',
});
const COULDN_T_CREATE_LINK_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when generating a shareable theme link fails.',
});
const THEME_TOO_LARGE_TITLE_DESCRIPTOR = msg({
	message: 'This theme is too large to share',
	comment: 'Title of the error modal shown when a theme CSS exceeds the maximum shareable size.',
});
const THEME_TOO_LARGE_MESSAGE_DESCRIPTOR = msg({
	message: 'Your theme CSS is too large to share. Trim it down and try again.',
	comment: 'Body of the error modal shown when a theme CSS exceeds the maximum shareable size.',
});
const GOING_TOO_FAST_TITLE_DESCRIPTOR = msg({
	message: "You're going too fast",
	comment: 'Title of the error modal shown when sharing or sending a theme link is rate limited.',
});
const GOING_TOO_FAST_MESSAGE_DESCRIPTOR = msg({
	message: 'Please wait a moment and try again.',
	comment: 'Body of the error modal shown when sharing or sending a theme link is rate limited.',
});

function pushThemeErrorModal(title: string, message: string): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal title={title} message={message} data-flx="theme.share-theme-modal.generic-error-modal" />
		)),
	);
}

function showCreateThemeLinkErrorModal(error: unknown): void {
	const code = failureCode(error);
	if (code === APIErrorCodes.FILE_SIZE_TOO_LARGE) {
		pushThemeErrorModal(
			i18nGlobal._(THEME_TOO_LARGE_TITLE_DESCRIPTOR),
			i18nGlobal._(THEME_TOO_LARGE_MESSAGE_DESCRIPTOR),
		);
		return;
	}
	if (code === APIErrorCodes.RATE_LIMITED) {
		pushThemeErrorModal(i18nGlobal._(GOING_TOO_FAST_TITLE_DESCRIPTOR), i18nGlobal._(GOING_TOO_FAST_MESSAGE_DESCRIPTOR));
		return;
	}
	pushThemeErrorModal(
		i18nGlobal._(COULDN_T_CREATE_LINK_TITLE_DESCRIPTOR),
		i18nGlobal._(COULDN_T_CREATE_LINK_MESSAGE_DESCRIPTOR),
	);
}

const logger = new Logger('ShareThemeModal');
export const ShareThemeModal = observer(({themeCss}: {themeCss: string}) => {
	const {i18n} = useLingui();
	const [themeUrl, setThemeUrl] = useState<string | null>(null);
	const [loading, setLoading] = useState(true);
	const [sentTo, setSentTo] = useState(new Map<string, boolean>());
	const [sendingTo, setSendingTo] = useState(new Set<string>());
	const recipients = useRecipientItems();
	const [searchQuery, setSearchQuery] = useState('');
	useEffect(() => {
		let cancelled = false;
		const createShareLink = async () => {
			setLoading(true);
			setThemeUrl(null);
			try {
				const response = await http.post<{id: string}>(Endpoints.USER_THEMES, {
					body: {
						css: themeCss,
					},
				});
				const themeId = response.body?.id;
				if (!themeId) {
					throw new Error('Missing theme id');
				}
				if (cancelled) return;
				const origin = RuntimeConfig.webAppBaseUrl;
				setThemeUrl(`${origin.replace(/\/$/, '')}${Routes.theme(themeId)}`);
			} catch (error) {
				logger.error('Failed to create theme share link:', error);
				if (!cancelled) {
					showCreateThemeLinkErrorModal(error);
				}
			} finally {
				if (!cancelled) {
					setLoading(false);
				}
			}
		};
		void createShareLink();
		return () => {
			cancelled = true;
		};
	}, [themeCss, RuntimeConfig.webAppBaseUrl]);
	const handleCopy = useCopyLinkHandler(themeUrl, true);
	const handleSendTheme = async (item: RecipientItem) => {
		if (!themeUrl) return;
		const userId = item.type === 'group_dm' ? item.id : item.user.id;
		setSendingTo((prev) => new Set(prev).add(userId));
		try {
			const targetChannelId = item.channelId
				? item.channelId
				: await PrivateChannelCommands.ensureDMChannel(item.user.id);
			const result = await MessageCommands.send(targetChannelId, {
				content: `${i18n._(CHECK_OUT_MY_CUSTOM_THEME_DESCRIPTOR)}\n${themeUrl}`,
				nonce: SnowflakeUtils.fromTimestamp(Date.now()),
			});
			if (result) {
				setSentTo((prev) => new Map(prev).set(userId, true));
			}
		} catch (error) {
			logger.error('Failed to send theme link:', error);
			showDmActionErrorModal(error);
		} finally {
			setSendingTo((prev) => {
				const next = new Set(prev);
				next.delete(userId);
				return next;
			});
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="theme.share-theme-modal.modal-root">
			<Modal.Header title={i18n._(SHARE_YOUR_THEME_DESCRIPTOR)} data-flx="theme.share-theme-modal.modal-header">
				<div className={selectorStyles.headerSearch} data-flx="theme.share-theme-modal.div">
					<Input
						value={searchQuery}
						onChange={(e) => setSearchQuery(e.target.value)}
						placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
						leftIcon={
							<MagnifyingGlassIcon
								size={remFromPx(20)}
								weight="bold"
								className={selectorStyles.searchIcon}
								data-flx="theme.share-theme-modal.magnifying-glass-icon"
							/>
						}
						className={selectorStyles.headerSearchInput}
						data-flx="theme.share-theme-modal.input.set-search-query"
					/>
				</div>
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent} data-flx="theme.share-theme-modal.modal-content">
				{loading ? (
					<div className={styles.loadingContainer} data-flx="theme.share-theme-modal.loading-container">
						<Spinner data-flx="theme.share-theme-modal.spinner" />
					</div>
				) : (
					<RecipientList
						recipients={recipients}
						sendingTo={sendingTo}
						sentTo={sentTo}
						onSend={handleSendTheme}
						defaultButtonLabel={i18n._(SEND_DESCRIPTOR)}
						sentButtonLabel={i18n._(SENT_DESCRIPTOR)}
						buttonClassName={styles.sendButton}
						scrollerKey="share-theme-modal-friend-list-scroller"
						searchQuery={searchQuery}
						onSearchQueryChange={setSearchQuery}
						showSearchInput={false}
						data-flx="theme.share-theme-modal.recipient-list"
					/>
				)}
			</Modal.Content>
			<Modal.Footer data-flx="theme.share-theme-modal.modal-footer">
				<CopyLinkSection
					label={<Trans>Or copy the link:</Trans>}
					value={themeUrl || ''}
					onCopy={handleCopy}
					onInputClick={(e) => e.currentTarget.select()}
					data-flx="theme.share-theme-modal.copy-link-section"
				/>
			</Modal.Footer>
		</Modal.Root>
	);
});
