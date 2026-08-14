// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CopyLinkSection} from '@app/features/app/components/dialogs/shared/CopyLinkSection';
import type {RecipientItem} from '@app/features/app/components/dialogs/shared/RecipientList';
import {RecipientList, useRecipientItems} from '@app/features/app/components/dialogs/shared/RecipientList';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import styles from '@app/features/expressions/components/modals/GiftSendToFriendModal.module.css';
import {SEARCH_FRIENDS_DESCRIPTOR, SENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Input} from '@app/features/ui/components/form/FormInput';
import {useCopyLinkHandler} from '@app/lib/copy-link';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const I_CAN_T_REDEEM_THIS_BECAUSE_I_ALREADY_DESCRIPTOR = msg({
	message: "I can't redeem this because I already have lifetime {premiumProductName}, so this gift is all yours!",
	comment: 'Gift redemption pre-filled DM message for a sender who already owns lifetime premium.',
});
const YOU_ALREADY_HAVE_LIFETIME_DESCRIPTOR = msg({
	message: 'You already have lifetime {premiumProductName}',
	comment: 'Status label shown to a user who already owns the lifetime premium tier.',
});
const SEND_GIFT_DESCRIPTOR = msg({
	message: 'Send gift',
	comment: 'Action label for sending a gift to a friend.',
});
const GIFT_LINK_DESCRIPTOR = msg({
	message: 'Gift link',
	comment: 'Form field label for a gift redemption URL.',
});
const logger = new Logger('GiftSendToFriendModal');

interface GiftSendToFriendModalProps {
	code: string;
}

export const GiftSendToFriendModal = observer(function GiftSendToFriendModal({code}: GiftSendToFriendModalProps) {
	const {i18n} = useLingui();
	const [sentTo, setSentTo] = useState(new Map<string, boolean>());
	const [sendingTo, setSendingTo] = useState(new Set<string>());
	const recipients = useRecipientItems();
	const [searchQuery, setSearchQuery] = useState('');
	const giftUrl = `${RuntimeConfig.giftEndpoint}/${code}`;
	const handleCopy = useCopyLinkHandler(giftUrl, true);
	const handleSendGift = useCallback(
		async (item: RecipientItem) => {
			const userId = item.type === 'group_dm' ? item.id : item.user.id;
			setSendingTo((previous) => new Set(previous).add(userId));
			try {
				const targetChannelId = item.channelId
					? item.channelId
					: await PrivateChannelCommands.ensureDMChannel(item.user.id);
				const result = await MessageCommands.send(targetChannelId, {
					content: `${i18n._(I_CAN_T_REDEEM_THIS_BECAUSE_I_ALREADY_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}\n${giftUrl}`,
					nonce: SnowflakeUtils.fromTimestamp(Date.now()),
				});
				if (result) {
					setSentTo((previous) => new Map(previous).set(userId, true));
				}
			} catch (error) {
				logger.error('Failed to send gift link:', error);
				showDmActionErrorModal(error);
			} finally {
				setSendingTo((previous) => {
					const next = new Set(previous);
					next.delete(userId);
					return next;
				});
			}
		},
		[giftUrl, i18n],
	);
	return (
		<Modal.Root size="small" centered data-flx="expressions.gift-send-to-friend-modal.modal-root">
			<Modal.Header
				title={i18n._(YOU_ALREADY_HAVE_LIFETIME_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
				data-flx="expressions.gift-send-to-friend-modal.modal-header"
			>
				<p className={styles.description} data-flx="expressions.gift-send-to-friend-modal.description">
					<Trans>
						We'd love to give you more than infinity of {PREMIUM_PRODUCT_NAME}, but that breaks the space-time
						continuum. Send the gift to a friend instead?
					</Trans>
				</p>
				<div className={selectorStyles.headerSearch} data-flx="expressions.gift-send-to-friend-modal.div">
					<Input
						value={searchQuery}
						onChange={(event) => setSearchQuery(event.target.value)}
						placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
						leftIcon={
							<MagnifyingGlassIcon
								size={remFromPx(20)}
								weight="bold"
								className={selectorStyles.searchIcon}
								data-flx="expressions.gift-send-to-friend-modal.magnifying-glass-icon"
							/>
						}
						className={selectorStyles.headerSearchInput}
						data-flx="expressions.gift-send-to-friend-modal.input.set-search-query"
					/>
				</div>
			</Modal.Header>
			<Modal.Content
				className={selectorStyles.selectorContent}
				data-flx="expressions.gift-send-to-friend-modal.modal-content"
			>
				<RecipientList
					recipients={recipients}
					sendingTo={sendingTo}
					sentTo={sentTo}
					onSend={handleSendGift}
					defaultButtonLabel={i18n._(SEND_GIFT_DESCRIPTOR)}
					sentButtonLabel={i18n._(SENT_DESCRIPTOR)}
					buttonClassName={styles.sendButton}
					scrollerKey="gift-send-to-friend-modal-friend-list-scroller"
					searchQuery={searchQuery}
					onSearchQueryChange={setSearchQuery}
					showSearchInput={false}
					data-flx="expressions.gift-send-to-friend-modal.recipient-list"
				/>
			</Modal.Content>
			<Modal.Footer data-flx="expressions.gift-send-to-friend-modal.modal-footer">
				<CopyLinkSection
					label={<Trans>or send the gift link to a friend:</Trans>}
					value={giftUrl}
					onCopy={handleCopy}
					onInputClick={(event) => event.currentTarget.select()}
					inputProps={{placeholder: i18n._(GIFT_LINK_DESCRIPTOR)}}
					data-flx="expressions.gift-send-to-friend-modal.copy-link-section"
				>
					<p className={styles.giftCodeText} data-flx="expressions.gift-send-to-friend-modal.gift-code-text">
						<Trans>Gift code:</Trans>{' '}
						<span className={styles.giftCode} data-flx="expressions.gift-send-to-friend-modal.gift-code">
							{code}
						</span>
					</p>
				</CopyLinkSection>
			</Modal.Footer>
		</Modal.Root>
	);
});
