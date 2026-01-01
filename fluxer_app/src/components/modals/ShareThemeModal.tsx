/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Input} from '~/components/form/Input';
import * as Modal from '~/components/modals/Modal';
import {CopyLinkSection} from '~/components/modals/shared/CopyLinkSection';
import type {RecipientItem} from '~/components/modals/shared/RecipientList';
import {RecipientList, useRecipientItems} from '~/components/modals/shared/RecipientList';
import selectorStyles from '~/components/modals/shared/SelectorModalStyles.module.css';
import {Spinner} from '~/components/uikit/Spinner';
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import {Routes} from '~/Routes';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import styles from './ShareThemeModal.module.css';

export const ShareThemeModal = observer(({themeCss}: {themeCss: string}) => {
	const {t, i18n} = useLingui();
	const [themeUrl, setThemeUrl] = React.useState<string | null>(null);
	const [loading, setLoading] = React.useState(true);
	const [copied, setCopied] = React.useState(false);
	const [sentTo, setSentTo] = React.useState(new Map<string, boolean>());
	const [sendingTo, setSendingTo] = React.useState(new Set<string>());
	const recipients = useRecipientItems();
	const [searchQuery, setSearchQuery] = React.useState('');

	React.useEffect(() => {
		let cancelled = false;

		const createShareLink = async () => {
			setLoading(true);
			setThemeUrl(null);

			try {
				const response = await HttpClient.post<{id: string}>({
					url: Endpoints.USER_THEMES,
					body: {
						css: themeCss,
					},
				});

				const themeId = response.body?.id;
				if (!themeId) {
					throw new Error('Missing theme id');
				}

				if (cancelled) return;

				const origin = RuntimeConfigStore.webAppBaseUrl;
				setThemeUrl(`${origin.replace(/\/$/, '')}${Routes.theme(themeId)}`);
			} catch (error) {
				console.error('Failed to create theme share link:', error);
				if (!cancelled) {
					ToastActionCreators.error(t`Failed to generate theme link.`);
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
	}, [themeCss, RuntimeConfigStore.webAppBaseUrl]);

	const handleCopy = async () => {
		if (!themeUrl) return;
		await TextCopyActionCreators.copy(i18n, themeUrl, true);
		setCopied(true);
		setTimeout(() => setCopied(false), 2000);
	};

	const handleSendTheme = async (item: RecipientItem) => {
		if (!themeUrl) return;

		const userId = item.type === 'group_dm' ? item.id : item.user.id;

		setSendingTo((prev) => new Set(prev).add(userId));
		try {
			let targetChannelId: string;
			if (item.channelId) {
				targetChannelId = item.channelId;
			} else {
				targetChannelId = await PrivateChannelActionCreators.ensureDMChannel(item.user.id);
			}

			await MessageActionCreators.send(targetChannelId, {
				content: `${t`Check out my custom theme!`}\n${themeUrl}`,
				nonce: SnowflakeUtils.fromTimestamp(Date.now()),
			});

			setSentTo((prev) => new Map(prev).set(userId, true));
		} catch (error) {
			console.error('Failed to send theme:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to send theme</Trans>,
			});
		} finally {
			setSendingTo((prev) => {
				const next = new Set(prev);
				next.delete(userId);
				return next;
			});
		}
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Share Your Theme`}>
				<div className={selectorStyles.headerSearch}>
					<Input
						value={searchQuery}
						onChange={(e) => setSearchQuery(e.target.value)}
						placeholder={t`Search friends`}
						leftIcon={<MagnifyingGlassIcon size={20} weight="bold" className={selectorStyles.searchIcon} />}
						className={selectorStyles.headerSearchInput}
					/>
				</div>
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent}>
				{loading ? (
					<div className={styles.loadingContainer}>
						<Spinner />
					</div>
				) : (
					<RecipientList
						recipients={recipients}
						sendingTo={sendingTo}
						sentTo={sentTo}
						onSend={handleSendTheme}
						defaultButtonLabel={t`Send`}
						sentButtonLabel={t`Sent`}
						buttonClassName={styles.sendButton}
						scrollerKey="share-theme-modal-friend-list-scroller"
						searchQuery={searchQuery}
						onSearchQueryChange={setSearchQuery}
						showSearchInput={false}
					/>
				)}
			</Modal.Content>
			<Modal.Footer>
				<CopyLinkSection
					label={<Trans>Or copy the link:</Trans>}
					value={themeUrl || ''}
					onCopy={handleCopy}
					copied={copied}
					onInputClick={(e) => e.currentTarget.select()}
				/>
			</Modal.Footer>
		</Modal.Root>
	);
});
