// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {
	type FormInputs,
	MAX_TOPIC_LENGTH,
	TOPIC_AUTOCOMPLETE_TRIGGERS,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import type {Channel} from '@app/features/channel/models/Channel';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {LexicalRichInput, type LexicalRichInputHandle} from '@app/features/lexical/composer/LexicalRichInput';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {convertMarkdownToSegments} from '@app/features/messaging/utils/MarkdownToSegmentUtils';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CharacterCounter} from '@app/features/ui/character_counter/CharacterCounter';
import formStyles from '@app/features/ui/components/form/FormInput.module.css';
import surfaceStyles from '@app/features/ui/components/form/FormSurface.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {setMeaningfulFormValue} from '@app/lib/forms/MeaningfulFormValue';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {forwardRef, useCallback, useEffect, useId, useImperativeHandle, useRef, useState} from 'react';
import type {UseFormReturn} from 'react-hook-form';

const TOPIC_DESCRIPTOR = msg({message: 'Topic'});
const ADD_A_TOPIC_TO_THIS_CHANNEL_DESCRIPTOR = msg({message: 'Add a topic to this channel'});
const INSERT_EMOJI_DESCRIPTOR = msg({message: 'Insert emoji'});
const TOPIC_MARKDOWN_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO);
const COMPOSER_SURFACE_INTERACTIVE_SELECTOR =
	'[data-channel-textarea], button, a, input, textarea, select, [role="button"]';

interface TopicHydration {
	display: string;
	segments: Array<MentionSegment>;
	key: number;
}

interface ChannelOverviewTopicEditorProps {
	channel: Channel;
	guildId: string | null;
	form: UseFormReturn<FormInputs>;
	initialTopic: string;
	onTopicExceedsLimit: () => void;
}

export interface ChannelOverviewTopicEditorHandle {
	syncFromMarkdown(markdown: string | null | undefined): void;
}

function buildHydration(rawTopic: string, guildId: string | null): TopicHydration {
	const converted = convertMarkdownToSegments(rawTopic, guildId);
	const segments = converted.segments.map((segment) => ({
		type: segment.type,
		id: segment.id,
		displayText: segment.displayText,
		actualText: segment.actualText,
		start: segment.start,
		end: segment.start + segment.displayText.length,
	}));
	return {display: converted.displayText, segments, key: 0};
}

export const ChannelOverviewTopicEditor = observer(
	forwardRef<ChannelOverviewTopicEditorHandle, ChannelOverviewTopicEditorProps>(
		({channel, guildId, form, initialTopic, onTopicExceedsLimit}, forwardedRef) => {
			const {i18n} = useLingui();
			const topicLabelId = useId();
			const topicErrorId = useId();
			const topicEditableId = useId();
			const composerRef = useRef<LexicalRichInputHandle | null>(null);
			const wrapperRef = useRef<HTMLDivElement | null>(null);
			const editableRef = useRef<HTMLElement | null>(null);
			const [topicExpressionPickerOpen, setTopicExpressionPickerOpen] = useState(false);
			const [topicHydration, setTopicHydration] = useState<TopicHydration>(() => buildHydration(initialTopic, guildId));
			const [actualTopic, setActualTopic] = useState(initialTopic);
			const [isTopicInitialized, setIsTopicInitialized] = useState(true);
			const originalTopicRef = useRef(initialTopic);
			const topicError = form.formState.errors.topic;
			const hasTopicError = topicError != null && topicError.message != null && topicError.message.length > 0;
			let topicErrorMessage: string | null = null;
			if (hasTopicError && topicError != null && topicError.message != null) {
				topicErrorMessage = topicError.message;
			}
			let topicErrorMessageId: string | null = null;
			if (hasTopicError) {
				topicErrorMessageId = topicErrorId;
			}
			let topicFieldClass = formStyles.focusable;
			if (hasTopicError) {
				topicFieldClass = formStyles.error;
			}
			const isMobile = MobileLayout.enabled;
			useEffect(() => {
				form.register('topic');
				return () => form.unregister('topic');
			}, [form]);
			useEffect(() => {
				const wrapper = wrapperRef.current;
				if (wrapper == null) {
					editableRef.current = null;
					return;
				}
				const editable = wrapper.querySelector<HTMLElement>('[data-channel-textarea]');
				if (editable == null) {
					editableRef.current = null;
					return;
				}
				editableRef.current = editable;
			}, [topicHydration.key]);
			const syncFromMarkdown = useCallback(
				(markdown: string | null | undefined) => {
					setIsTopicInitialized(false);
					let rawTopic = '';
					if (markdown != null) {
						rawTopic = markdown;
					}
					const next = buildHydration(rawTopic, guildId);
					originalTopicRef.current = rawTopic;
					setActualTopic(rawTopic);
					setTopicHydration((previous) => ({display: next.display, segments: next.segments, key: previous.key + 1}));
					form.setValue('topic', rawTopic, {shouldDirty: false, shouldTouch: false});
					setIsTopicInitialized(true);
				},
				[form, guildId],
			);
			useImperativeHandle(forwardedRef, () => ({syncFromMarkdown}), [syncFromMarkdown]);
			useEffect(() => {
				if (!isTopicInitialized) {
					return;
				}
				setMeaningfulFormValue({
					setValue: form.setValue,
					name: 'topic',
					currentValue: actualTopic,
					cleanValue: originalTopicRef.current,
					isMeaningfullyDirty: actualTopic !== originalTopicRef.current,
				});
			}, [actualTopic, form, isTopicInitialized]);
			const handleTopicChange = useCallback((_display: string, _segments: Array<MentionSegment>, wire: string) => {
				setActualTopic(wire);
			}, []);
			const handleTopicEmojiSelect = useCallback((emoji: FlatEmoji, shiftKey: boolean) => {
				const composer = composerRef.current;
				if (composer == null) {
					return false;
				}
				const didInsert = composer.insertEmoji(emoji);
				if (didInsert && shiftKey !== true) {
					setTopicExpressionPickerOpen(false);
				}
				return didInsert;
			}, []);
			const emojiButton = isMobile ? (
				<FocusRing offset={-2}>
					<button
						type="button"
						onClick={() => setTopicExpressionPickerOpen(true)}
						className={clsx(
							styles.emojiButton,
							topicExpressionPickerOpen ? styles.emojiButtonActive : styles.emojiButtonInactive,
						)}
						aria-label={i18n._(INSERT_EMOJI_DESCRIPTOR)}
						aria-haspopup="dialog"
						aria-expanded={topicExpressionPickerOpen}
					>
						<SmileyIcon size={remFromPx(20)} weight="fill" />
					</button>
				</FocusRing>
			) : (
				<Popout
					position="bottom-end"
					animationType="none"
					offsetMainAxis={8}
					offsetCrossAxis={-32}
					onOpen={() => setTopicExpressionPickerOpen(true)}
					onClose={() => setTopicExpressionPickerOpen(false)}
					returnFocusRef={editableRef}
					render={({onClose}) => (
						<ExpressionPickerPopout
							channelId={channel.id}
							onEmojiSelect={(emoji, shiftKey) => {
								const didInsert = handleTopicEmojiSelect(emoji, shiftKey === true);
								if (didInsert && shiftKey !== true) {
									onClose();
								}
							}}
							onClose={onClose}
							visibleTabs={['emojis']}
						/>
					)}
				>
					<FocusRing offset={-2}>
						<button
							type="button"
							className={clsx(
								styles.emojiButton,
								topicExpressionPickerOpen ? styles.emojiButtonActive : styles.emojiButtonInactive,
							)}
							aria-label={i18n._(INSERT_EMOJI_DESCRIPTOR)}
							aria-haspopup="dialog"
							aria-expanded={topicExpressionPickerOpen}
						>
							<SmileyIcon size={remFromPx(20)} weight="fill" />
						</button>
					</FocusRing>
				</Popout>
			);
			return (
				<>
					<fieldset className={formStyles.fieldset}>
						<div className={formStyles.labelContainer}>
							<label
								id={topicLabelId}
								className={formStyles.label}
								htmlFor={topicEditableId}
								onPointerDown={(event) => {
									event.preventDefault();
									const composer = composerRef.current;
									if (composer != null) {
										composer.focus();
									}
								}}
							>
								{i18n._(TOPIC_DESCRIPTOR)}
							</label>
						</div>
						<div className={formStyles.inputGroup}>
							<FocusRing within={true} ringTarget={wrapperRef} focusTarget={wrapperRef} offset={-2}>
								<div
									ref={wrapperRef}
									className={clsx(formStyles.textareaWrapper, surfaceStyles.surface, topicFieldClass)}
									onPointerDown={(event) => {
										const target = event.target;
										if (target instanceof Element && target.closest(COMPOSER_SURFACE_INTERACTIVE_SELECTOR) != null) {
											return;
										}
										event.preventDefault();
										const composer = composerRef.current;
										if (composer != null) {
											composer.focus();
										}
									}}
								>
									<LexicalRichInput
										key={topicHydration.key}
										initialValue={topicHydration.display}
										initialSegments={topicHydration.segments}
										placeholder={i18n._(ADD_A_TOPIC_TO_THIS_CHANNEL_DESCRIPTOR)}
										disabled={false}
										channel={channel}
										allowedTriggers={[...TOPIC_AUTOCOMPLETE_TRIGGERS]}
										markdown={true}
										markdownParserFlags={TOPIC_MARKDOWN_PARSER_FLAGS}
										singleLine={false}
										size="form"
										maxLength={MAX_TOPIC_LENGTH}
										onExceedMaxLength={onTopicExceedsLimit}
										className={styles.topicInput}
										autocompleteAnchor={wrapperRef.current}
										id={topicEditableId}
										ariaLabelledBy={topicLabelId}
										ariaInvalid={hasTopicError}
										ariaErrorMessage={topicErrorMessageId == null ? undefined : topicErrorMessageId}
										ariaDescribedBy={topicErrorMessageId == null ? undefined : topicErrorMessageId}
										richInputRef={composerRef}
										onChange={handleTopicChange}
										i18n={i18n}
									/>
									<div className={formStyles.textareaActions}>
										{emojiButton}
										<div className={formStyles.characterCountContainer}>
											<CharacterCounter
												currentLength={actualTopic.length}
												maxLength={MAX_TOPIC_LENGTH}
												canUpgrade={false}
												premiumMaxLength={MAX_TOPIC_LENGTH}
												onUpgradeClick={() => undefined}
											/>
										</div>
									</div>
								</div>
							</FocusRing>
							{hasTopicError && (
								<span id={topicErrorId} className={formStyles.errorText}>
									{topicErrorMessage}
								</span>
							)}
						</div>
					</fieldset>
					{isMobile && (
						<ExpressionPickerSheet
							isOpen={topicExpressionPickerOpen}
							onClose={() => setTopicExpressionPickerOpen(false)}
							onEmojiSelect={(emoji, shiftKey) => {
								handleTopicEmojiSelect(emoji, shiftKey === true);
							}}
							visibleTabs={['emojis']}
							channelId={channel.id}
						/>
					)}
				</>
			);
		},
	),
);
