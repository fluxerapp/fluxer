// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {WHATS_NEW_ENTRIES, type WhatsNewEntry} from '@app/features/app/components/whats_new/WhatsNewEntries';
import styles from '@app/features/app/components/whats_new/WhatsNewModal.module.css';
import {BLUESKY_PROVIDER_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {BlueskyIcon} from '@app/features/ui/components/icons/BlueskyIcon';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import WhatsNew from '@app/features/ui/state/WhatsNew';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {ExternalUrls} from '@fluxer/constants/src/ExternalUrls';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon} from '@phosphor-icons/react';
import {useCallback, useEffect, useRef, useState} from 'react';

const WHAT_S_NEW_DESCRIPTOR = msg({
	message: "What's new",
	comment: 'Short label in the whats new modal.',
});
const SCROLL_TO_BOTTOM_DESCRIPTOR = msg({
	message: 'Scroll to bottom',
	comment: 'Short label in the whats new modal.',
});
const FOLLOW_US_ON_DESCRIPTOR = msg({
	message: 'Follow us on {blueskyProviderName}',
	comment: 'Short label in the whats new modal. Preserve {blueskyProviderName}; it is inserted by code.',
});
const GOT_IT_DESCRIPTOR = msg({
	message: 'Got it',
	comment: 'Short label in the whats new modal.',
});
const WHATS_NEW_MODAL_KEY = 'whats-new';

export function openWhatsNewModal(): void {
	const latestEntry = WHATS_NEW_ENTRIES[0];
	if (!latestEntry) return;
	ModalCommands.pushWithKey(
		modal(() => (
			<WhatsNewModal
				entry={latestEntry}
				data-flx="app.whats-new.whats-new-modal.open-whats-new-modal.whats-new-modal"
			/>
		)),
		WHATS_NEW_MODAL_KEY,
	);
}

interface WhatsNewModalProps {
	entry: WhatsNewEntry;
}

function formatEntryDate(date: Date): string {
	return getFormattedLongDate(date, getCurrentLocale());
}

export function WhatsNewModal({entry}: WhatsNewModalProps) {
	const {i18n} = useLingui();
	const initialFocusRef = useRef<HTMLDivElement | null>(null);
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const [isScrollHintVisible, setIsScrollHintVisible] = useState(false);
	const handleDismiss = useCallback(() => {
		WhatsNew.dismiss(entry.id);
		ModalCommands.pop();
	}, [entry.id]);
	const updateScrollHintVisibility = useCallback(() => {
		const scrollerNode = scrollerRef.current?.getScrollerNode();
		if (!scrollerNode) {
			setIsScrollHintVisible(false);
			return;
		}
		const maxScroll = Math.max(0, scrollerNode.scrollHeight - scrollerNode.clientHeight);
		const distanceFromBottom = maxScroll - scrollerNode.scrollTop;
		const shouldShowScrollHint = maxScroll > 16 && scrollerNode.scrollTop < 72 && distanceFromBottom > 24;
		setIsScrollHintVisible(shouldShowScrollHint);
	}, []);
	const handleContentScroll = useCallback(() => {
		updateScrollHintVisibility();
	}, [updateScrollHintVisibility]);
	const handleContentResize = useCallback(() => {
		updateScrollHintVisibility();
	}, [updateScrollHintVisibility]);
	const handleScrollToBottom = useCallback(() => {
		scrollerRef.current?.scrollToBottom({animate: true});
	}, []);
	useEffect(() => {
		updateScrollHintVisibility();
	}, [entry.id, updateScrollHintVisibility]);
	return (
		<Modal.Root
			size="medium"
			className={styles.whatsNewRoot}
			centered
			onClose={handleDismiss}
			initialFocusRef={initialFocusRef}
			data-flx="app.whats-new.whats-new-modal.whats-new-root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(WHAT_S_NEW_DESCRIPTOR)}
				data-flx="app.whats-new.whats-new-modal.modal-screen-reader-label"
			/>
			<div className={styles.contentShell} data-flx="app.whats-new.whats-new-modal.content-shell">
				<Modal.Content
					padding="none"
					ref={scrollerRef}
					fade={false}
					onScroll={handleContentScroll}
					onResize={handleContentResize}
					data-flx="app.whats-new.whats-new-modal.modal-content"
				>
					<div
						className={styles.contentBody}
						ref={initialFocusRef}
						tabIndex={-1}
						data-flx="app.whats-new.whats-new-modal.content-body"
					>
						<img
							src={entry.coverImage}
							alt=""
							width={1920}
							height={1080}
							className={styles.coverImage}
							data-flx="app.whats-new.whats-new-modal.cover-image"
						/>
						<div className={styles.dateLine} data-flx="app.whats-new.whats-new-modal.date-line">
							{formatEntryDate(entry.date)}
						</div>
						<div
							className={`${markupStyles.markup} ${styles.markdownBody}`}
							data-flx="app.whats-new.whats-new-modal.markdown-body"
						>
							<SafeMarkdown
								content={entry.content}
								options={{context: MarkdownContext.STANDARD_WITHOUT_JUMBO}}
								data-flx="app.whats-new.whats-new-modal.safe-markdown"
							/>
						</div>
					</div>
				</Modal.Content>
				<button
					type="button"
					className={isScrollHintVisible ? `${styles.scrollHint} ${styles.scrollHintVisible}` : styles.scrollHint}
					aria-hidden={!isScrollHintVisible}
					aria-label={i18n._(SCROLL_TO_BOTTOM_DESCRIPTOR)}
					tabIndex={isScrollHintVisible ? 0 : -1}
					onClick={handleScrollToBottom}
					data-flx="app.whats-new.whats-new-modal.scroll-hint.scroll-to-bottom.button"
				>
					<div className={styles.scrollHintCircle} data-flx="app.whats-new.whats-new-modal.scroll-hint-circle">
						<CaretDownIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="app.whats-new.whats-new-modal.caret-down-icon"
						/>
					</div>
				</button>
			</div>
			<Modal.Footer className={styles.footer} data-flx="app.whats-new.whats-new-modal.footer">
				<a
					href={ExternalUrls.BLUESKY}
					target="_blank"
					rel="noopener noreferrer"
					className={styles.blueskyLink}
					data-flx="app.whats-new.whats-new-modal.bluesky-link"
				>
					<BlueskyIcon size={16} className={styles.blueskyIcon} data-flx="app.whats-new.whats-new-modal.bluesky-icon" />
					{i18n._(FOLLOW_US_ON_DESCRIPTOR, {blueskyProviderName: BLUESKY_PROVIDER_NAME})}
				</a>
				<Button
					variant="primary"
					fitContent
					className={styles.dismissButton}
					onClick={handleDismiss}
					data-flx="app.whats-new.whats-new-modal.dismiss-button"
				>
					{i18n._(GOT_IT_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
}
