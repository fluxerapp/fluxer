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

import * as PopoutActionCreators from '@app/actions/PopoutActionCreators';
import {Autocomplete} from '@app/components/channel/Autocomplete';
import editingStyles from '@app/components/channel/EditingMessageInput.module.css';
import {MessageCharacterCounter} from '@app/components/channel/MessageCharacterCounter';
import {TextareaButton} from '@app/components/channel/textarea/TextareaButton';
import styles from '@app/components/channel/textarea/TextareaInput.module.css';
import {TextareaInputField} from '@app/components/channel/textarea/TextareaInputField';
import {ExpressionPickerSheet} from '@app/components/modals/ExpressionPickerSheet';
import {ExpressionPickerPopout} from '@app/components/popouts/ExpressionPickerPopout';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {openPopout} from '@app/components/uikit/popout/Popout';
import {Scroller, type ScrollerHandle} from '@app/components/uikit/Scroller';
import {useMarkdownKeybinds} from '@app/hooks/useMarkdownKeybinds';
import {useTextareaAutocomplete} from '@app/hooks/useTextareaAutocomplete';
import {useTextareaEmojiPicker} from '@app/hooks/useTextareaEmojiPicker';
import {useTextareaPaste} from '@app/hooks/useTextareaPaste';
import {useTextareaSegments} from '@app/hooks/useTextareaSegments';
import {ComponentDispatch} from '@app/lib/ComponentDispatch';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import UserStore from '@app/stores/UserStore';
import {Limits} from '@app/utils/limits/UserLimits';
import {applyMarkdownSegments} from '@app/utils/MarkdownToSegmentUtils';
import {MAX_MESSAGE_LENGTH_NON_PREMIUM, MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

export const EditingMessageInput = observer(
	({
		channel,
		onCancel,
		onSubmit,
		textareaRef,
		value,
		setValue,
	}: {
		channel: ChannelRecord;
		onCancel: () => void;
		onSubmit: (actualContent?: string) => void;
		textareaRef: React.RefObject<HTMLTextAreaElement | null>;
		value: string;
		setValue: React.Dispatch<React.SetStateAction<string>>;
	}) => {
		const {t} = useLingui();
		const currentUser = UserStore.getCurrentUser();
		const maxMessageLength = currentUser?.maxMessageLength ?? MAX_MESSAGE_LENGTH_NON_PREMIUM;
		const premiumMaxLength = Limits.getPremiumValue('max_message_length', MAX_MESSAGE_LENGTH_PREMIUM);
		const [expressionPickerOpen, setExpressionPickerOpen] = useState(false);
		const hasInitializedRef = useRef(false);
		const containerRef = useRef<HTMLDivElement>(null);
		const scrollerRef = useRef<ScrollerHandle>(null);
		const mobileLayout = MobileLayoutStore;
		const expressionPickerTriggerRef = useRef<HTMLButtonElement>(null);
		const [isFocused, setIsFocused] = useState(false);
		useMarkdownKeybinds(isFocused);
		const [textareaHeight, setTextareaHeight] = useState(0);
		const hasScrolledInitiallyRef = useRef(false);
		const shouldStickToBottomRef = useRef(true);
		const previousHeightRef = useRef<number | null>(null);

		const handleScroll = useCallback(() => {
			const distance = scrollerRef.current?.getDistanceFromBottom?.();
			if (distance == null) return;
			shouldStickToBottomRef.current = distance <= 8;
		}, []);
		const handleTextareaKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
				if (event.key === 'Escape') {
					event.preventDefault();
					event.stopPropagation();
					onCancel();
				}
			},
			[onCancel],
		);

		const {segmentManagerRef, previousValueRef, displayToActual, handleTextChange} = useTextareaSegments();
		const {handleEmojiSelect} = useTextareaEmojiPicker({setValue, textareaRef, segmentManagerRef, previousValueRef});
		const actualContent = useMemo(() => displayToActual(value), [displayToActual, value]);

		const {
			autocompleteQuery,
			autocompleteOptions,
			autocompleteType,
			selectedIndex,
			isAutocompleteAttached,
			setSelectedIndex,
			onCursorMove,
			handleSelect,
		} = useTextareaAutocomplete({
			channel,
			value,
			setValue,
			textareaRef,
			segmentManagerRef,
			previousValueRef,
			allowedTriggers: ['emoji', 'mention'],
		});

		useTextareaPaste({
			channel,
			textareaRef,
			segmentManagerRef,
			setValue,
			previousValueRef,
		});

		useLayoutEffect(() => {
			if (!hasInitializedRef.current && value) {
				hasInitializedRef.current = true;

				const displayText = applyMarkdownSegments(value, channel.guildId, segmentManagerRef.current);

				setValue(displayText);
				previousValueRef.current = displayText;

				requestAnimationFrame(() => {
					if (textareaRef.current) {
						const length = displayText.length;
						textareaRef.current.setSelectionRange(length, length);
					}
				});
			}
		}, [value, channel.guildId, setValue, segmentManagerRef, previousValueRef]);

		useLayoutEffect(() => {
			if (hasScrolledInitiallyRef.current) return;
			if (!scrollerRef.current) return;
			if (textareaHeight <= 0) return;

			scrollerRef.current.scrollToBottom({animate: false});
			hasScrolledInitiallyRef.current = true;
			shouldStickToBottomRef.current = true;
		}, [textareaHeight]);

		useLayoutEffect(() => {
			const container = containerRef.current;
			if (!container) return;

			const ro = new ResizeObserver((entries) => {
				const entry = entries[0];
				if (!entry) return;

				const height = entry.borderBoxSize?.[0]?.blockSize ?? entry.contentRect.height;
				const prevHeight = previousHeightRef.current;
				previousHeightRef.current = height;

				if (prevHeight != null && prevHeight !== height) {
					const delta = height - prevHeight;
					ComponentDispatch.dispatch('LAYOUT_RESIZED', {
						channelId: channel.id,
						heightDelta: delta,
					});
				}
			});

			ro.observe(container);
			return () => ro.disconnect();
		}, [channel.id]);

		const handleSubmit = useCallback(() => {
			if (actualContent.length > maxMessageLength) {
				return;
			}
			onSubmit(actualContent);
		}, [actualContent, onSubmit, maxMessageLength]);

		const handleExpressionPickerToggle = useCallback(() => {
			const triggerElement = expressionPickerTriggerRef.current;
			if (!triggerElement) return;

			const popoutKey = `editing-expression-picker-${channel.id}`;
			const isOpen = expressionPickerOpen;

			if (isOpen) {
				PopoutActionCreators.close(popoutKey);
				setExpressionPickerOpen(false);
			} else {
				openPopout(
					triggerElement,
					{
						render: ({onClose}) => (
							<ExpressionPickerPopout
								channelId={channel.id}
								onEmojiSelect={handleEmojiSelect}
								onClose={onClose}
								visibleTabs={['emojis']}
							/>
						),
						position: 'top-end',
						animationType: 'none',
						offsetCrossAxis: 16,
						onOpen: () => setExpressionPickerOpen(true),
						onClose: () => setExpressionPickerOpen(false),
						returnFocusRef: textareaRef,
					},
					popoutKey,
				);
			}
		}, [channel.id, expressionPickerOpen, handleEmojiSelect, textareaRef]);

		return (
			<>
				{isAutocompleteAttached && (
					<Autocomplete
						type={autocompleteType}
						onSelect={handleSelect}
						selectedIndex={selectedIndex}
						options={autocompleteOptions}
						setSelectedIndex={setSelectedIndex}
						referenceElement={containerRef.current}
						query={autocompleteQuery}
					/>
				)}

				<FocusRing within={true} offset={-2}>
					<div ref={containerRef} className={styles.textareaContainer}>
						<div className={styles.mainWrapperEditing}>
							<div className={styles.contentAreaEditing}>
								<Scroller
									ref={scrollerRef}
									fade={true}
									className={editingStyles.scroller}
									key="editing-message-input-scroller"
									onScroll={handleScroll}
								>
									<div className={editingStyles.flexColumnContainer}>
										<span key={textareaHeight} className={editingStyles.hiddenSpan} />
										<TextareaInputField
											channelId={channel.id}
											disabled={false}
											isMobile={mobileLayout.enabled}
											value={value}
											placeholder=""
											textareaRef={textareaRef}
											scrollerRef={scrollerRef}
											shouldStickToBottomRef={shouldStickToBottomRef}
											isFocused={isFocused}
											isAutocompleteAttached={isAutocompleteAttached}
											autocompleteOptions={autocompleteOptions}
											selectedIndex={selectedIndex}
											onFocus={() => setIsFocused(true)}
											onBlur={() => setIsFocused(false)}
											onChange={(newValue) => {
												handleTextChange(newValue, previousValueRef.current);
												setValue(newValue);
											}}
											onHeightChange={setTextareaHeight}
											onCursorMove={onCursorMove}
											onArrowUp={() => {}}
											onEnter={handleSubmit}
											onAutocompleteSelect={handleSelect}
											setSelectedIndex={setSelectedIndex}
											onKeyDown={handleTextareaKeyDown}
										/>
									</div>
								</Scroller>
							</div>

							<div className={styles.buttonContainerEditing}>
								<TextareaButton
									ref={mobileLayout.enabled ? undefined : expressionPickerTriggerRef}
									icon={SmileyIcon}
									iconProps={{weight: 'fill'}}
									label={t`Emojis`}
									isSelected={expressionPickerOpen}
									onClick={mobileLayout.enabled ? () => setExpressionPickerOpen(true) : handleExpressionPickerToggle}
									data-expression-picker-tab="emojis"
									compact={true}
								/>
							</div>
						</div>

						<MessageCharacterCounter
							currentLength={actualContent.length}
							maxLength={maxMessageLength}
							canUpgrade={maxMessageLength < premiumMaxLength}
							premiumMaxLength={premiumMaxLength}
						/>
					</div>
				</FocusRing>

				<div className={editingStyles.footer}>
					<div>
						<Trans>
							escape to{' '}
							<FocusRing offset={-2}>
								<button type="button" className={editingStyles.footerLink} onClick={onCancel} key="cancel">
									cancel
								</button>
							</FocusRing>
						</Trans>
						<div aria-hidden={true} className={editingStyles.separator} />
						<Trans>
							enter to{' '}
							<FocusRing offset={-2}>
								<button type="button" className={editingStyles.footerLink} onClick={handleSubmit} key="save">
									save
								</button>
							</FocusRing>
						</Trans>
					</div>
				</div>

				{mobileLayout.enabled && (
					<ExpressionPickerSheet
						isOpen={expressionPickerOpen}
						onClose={() => setExpressionPickerOpen(false)}
						channelId={channel.id}
						onEmojiSelect={handleEmojiSelect}
						visibleTabs={['emojis']}
						selectedTab="emojis"
					/>
				)}
			</>
		);
	},
);
