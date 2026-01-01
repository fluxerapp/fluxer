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
import {SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Controller, useForm} from 'react-hook-form';
import {components, type OptionProps, type SingleValueProps} from 'react-select';
import type {ChannelRtcRegion} from '~/actions/ChannelActionCreators';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {ChannelTypes, Permissions} from '~/Constants';
import {Autocomplete} from '~/components/channel/Autocomplete';
import {Form} from '~/components/form/Form';
import {Input, Textarea} from '~/components/form/Input';
import {Select as FormSelect} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import {ExpressionPickerSheet} from '~/components/modals/ExpressionPickerSheet';
import {ExpressionPickerPopout} from '~/components/popouts/ExpressionPickerPopout';
import {CharacterCounter} from '~/components/uikit/CharacterCounter/CharacterCounter';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Popout} from '~/components/uikit/Popout/Popout';
import {Slider} from '~/components/uikit/Slider';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import {useMarkdownKeybinds} from '~/hooks/useMarkdownKeybinds';
import {type TriggerType, useTextareaAutocomplete} from '~/hooks/useTextareaAutocomplete';
import {useTextareaAutocompleteKeyboard} from '~/hooks/useTextareaAutocompleteKeyboard';
import {useTextareaEmojiPicker} from '~/hooks/useTextareaEmojiPicker';
import {useTextareaPaste} from '~/hooks/useTextareaPaste';
import {useTextareaSegments} from '~/hooks/useTextareaSegments';
import ChannelStore from '~/stores/ChannelStore';
import type {Emoji} from '~/stores/EmojiStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import * as EmojiUtils from '~/utils/EmojiUtils';
import {applyMarkdownSegments} from '~/utils/MarkdownToSegmentUtils';
import styles from './ChannelOverviewTab.module.css';

interface FormInputs {
	name: string;
	topic?: string;
	url?: string;
	slowmode?: number;
	nsfw?: boolean;
	bitrate?: number;
	user_limit?: number;
	rtc_region: string | null;
}

const CHANNEL_OVERVIEW_TAB_ID = 'overview';
const SETTINGS_AUTOCOMPLETE_Z_INDEX = 10001;

const BITRATE_OPTIONS = [8, 64, 96, 128] as const;
const MAX_TOPIC_LENGTH = 1024;
const TOPIC_AUTOCOMPLETE_TRIGGERS: Array<TriggerType> = ['emoji'];

const getNearestBitrate = (value: number): number => {
	return BITRATE_OPTIONS.reduce((closest, option) => {
		return Math.abs(option - value) < Math.abs(closest - value) ? option : closest;
	});
};

interface RtcRegionOption {
	value: string | null;
	label: string;
	region: ChannelRtcRegion | null;
}

const RtcRegionOptionComponent = observer((props: OptionProps<RtcRegionOption>) => {
	const {region, label} = props.data as RtcRegionOption;
	if (!region) {
		return (
			<components.Option {...props}>
				<span>{label}</span>
			</components.Option>
		);
	}

	const displayName = (props as any).getRegionDisplayName(region.id, region.name);
	return (
		<components.Option {...props}>
			<div className={styles.regionOption}>
				<img src={EmojiUtils.getEmojiURL(region.emoji) ?? undefined} alt={displayName} className={styles.regionEmoji} />
				<span>{displayName}</span>
			</div>
		</components.Option>
	);
});

const RtcRegionSingleValue = observer((props: SingleValueProps<RtcRegionOption>) => {
	const {region, label} = props.data as RtcRegionOption;
	if (!region) {
		return (
			<components.SingleValue {...props}>
				<span>{label}</span>
			</components.SingleValue>
		);
	}

	const displayName = (props as any).getRegionDisplayName(region.id, region.name);
	return (
		<components.SingleValue {...props}>
			<div className={styles.regionOption}>
				<img src={EmojiUtils.getEmojiURL(region.emoji) ?? undefined} alt={displayName} className={styles.regionEmoji} />
				<span>{displayName}</span>
			</div>
		</components.SingleValue>
	);
});

const ChannelOverviewTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {t} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const mobileLayout = MobileLayoutStore;
	const guildId = channel?.guildId ?? null;
	const canUpdateRtcRegion =
		guildId !== null ? PermissionStore.can(Permissions.UPDATE_RTC_REGION, {guildId, channelId}) : false;
	const isVoiceChannel = channel?.type === ChannelTypes.GUILD_VOICE;
	const [rtcRegions, setRtcRegions] = React.useState<Array<ChannelRtcRegion>>([]);
	const [isLoadingRegions, setIsLoadingRegions] = React.useState(false);

	const slowmodeOptions = React.useMemo(
		() => [
			{value: 0, label: t`Off`},
			{value: 5, label: t`5 seconds`},
			{value: 10, label: t`10 seconds`},
			{value: 15, label: t`15 seconds`},
			{value: 30, label: t`30 seconds`},
			{value: 60, label: t`1 minute`},
			{value: 120, label: t`2 minutes`},
			{value: 300, label: t`5 minutes`},
			{value: 600, label: t`10 minutes`},
			{value: 900, label: t`15 minutes`},
			{value: 1800, label: t`30 minutes`},
			{value: 3600, label: t`1 hour`},
			{value: 7200, label: t`2 hours`},
			{value: 21600, label: t`6 hours`},
		],
		[t],
	);

	const automaticLabel = React.useMemo(() => t`Automatic`, [t]);

	const getRegionDisplayName = React.useCallback(
		(regionId: string, regionName: string): string => {
			if (regionId === 'us-east') {
				return t`US East`;
			}
			if (regionId === 'eu-central') {
				return t`EU Central`;
			}
			return regionName;
		},
		[t],
	);

	const form = useForm<FormInputs>({
		defaultValues: {
			name: '',
			topic: '',
			url: '',
			slowmode: 0,
			nsfw: false,
			bitrate: 64,
			user_limit: 0,
			rtc_region: null,
		},
	});

	React.useEffect(() => {
		if (!canUpdateRtcRegion || !isVoiceChannel) {
			setRtcRegions([]);
			setIsLoadingRegions(false);
			return;
		}

		let cancelled = false;
		setIsLoadingRegions(true);
		ChannelActionCreators.fetchRtcRegions(channelId)
			.then((regions) => {
				if (cancelled) return;
				setRtcRegions(regions);
			})
			.catch(() => {
				if (cancelled) return;
				setRtcRegions([]);
				ToastActionCreators.error(t`Failed to load voice regions for this channel.`);
			})
			.finally(() => {
				if (cancelled) return;
				setIsLoadingRegions(false);
			});

		return () => {
			cancelled = true;
		};
	}, [canUpdateRtcRegion, channelId, isVoiceChannel]);

	React.useEffect(() => {
		if (!channel) return;
		form.reset({
			name: channel.name || '',
			topic: channel.topic || '',
			url: channel.url || '',
			slowmode: channel.rateLimitPerUser || 0,
			nsfw: channel.nsfw || false,
			bitrate: channel.bitrate ? getNearestBitrate(Math.round(channel.bitrate / 1000)) : 64,
			user_limit: channel.userLimit ?? 0,
			rtc_region: channel.rtcRegion ?? null,
		});
	}, [channel, form]);

	React.useEffect(() => {
		if (!canUpdateRtcRegion || !isVoiceChannel || rtcRegions.length === 0) {
			return;
		}
		const currentValue = form.getValues('rtc_region');
		if (currentValue && !rtcRegions.some((region) => region.id === currentValue)) {
			form.setValue('rtc_region', null, {shouldDirty: false, shouldTouch: false});
		}
	}, [canUpdateRtcRegion, form, isVoiceChannel, rtcRegions]);

	React.useEffect(() => {
		form.register('topic');
		return () => {
			form.unregister('topic');
		};
	}, [form]);

	const topicTextareaRef = React.useRef<HTMLTextAreaElement | null>(null);
	const {segmentManagerRef, previousValueRef, displayToActual, insertSegment, handleTextChange, clearSegments} =
		useTextareaSegments();
	const [topicValue, setTopicValue] = React.useState('');
	const [isTopicInitialized, setIsTopicInitialized] = React.useState(false);
	const originalTopicRef = React.useRef('');
	const [topicExpressionPickerOpen, setTopicExpressionPickerOpen] = React.useState(false);
	const [isTopicFieldFocused, setIsTopicFieldFocused] = React.useState(false);
	useMarkdownKeybinds(isTopicFieldFocused);

	const {handleEmojiSelect: insertTopicEmoji} = useTextareaEmojiPicker({
		setValue: setTopicValue,
		textareaRef: topicTextareaRef,
		insertSegment,
		previousValueRef,
		channelId,
	});

	const {
		autocompleteQuery: topicAutocompleteQuery,
		autocompleteOptions: topicAutocompleteOptions,
		autocompleteType: topicAutocompleteType,
		selectedIndex: topicSelectedIndex,
		isAutocompleteAttached: topicIsAutocompleteAttached,
		setSelectedIndex: topicSetSelectedIndex,
		onCursorMove: topicOnCursorMove,
		handleSelect: topicHandleSelect,
	} = useTextareaAutocomplete({
		channel: channel ?? null,
		value: topicValue,
		setValue: setTopicValue,
		textareaRef: topicTextareaRef,
		segmentManagerRef,
		previousValueRef,
		allowedTriggers: TOPIC_AUTOCOMPLETE_TRIGGERS,
	});

	useTextareaPaste({
		channel: channel ?? null,
		textareaRef: topicTextareaRef,
		segmentManagerRef,
		setValue: setTopicValue,
		previousValueRef,
	});

	const topicContainerRef = React.useRef<HTMLDivElement>(null);

	const {handleKeyDown: handleTopicKeyDown} = useTextareaAutocompleteKeyboard({
		isAutocompleteAttached: topicIsAutocompleteAttached,
		autocompleteOptions: topicAutocompleteOptions,
		selectedIndex: topicSelectedIndex,
		setSelectedIndex: topicSetSelectedIndex,
		handleSelect: topicHandleSelect,
	});

	const handleTopicEmojiSelect = React.useCallback(
		(emoji: Emoji, shiftKey?: boolean) => {
			insertTopicEmoji(emoji, shiftKey);
			if (!shiftKey) {
				setTopicExpressionPickerOpen(false);
			}
		},
		[insertTopicEmoji],
	);

	const syncTopicFromMarkdown = React.useCallback(
		(markdown: string | null | undefined) => {
			setIsTopicInitialized(false);
			clearSegments();
			const rawTopic = markdown ?? '';
			const displayTopic = rawTopic ? applyMarkdownSegments(rawTopic, guildId, segmentManagerRef.current) : '';
			originalTopicRef.current = rawTopic;
			previousValueRef.current = displayTopic;
			setTopicValue(displayTopic);
			form.setValue('topic', rawTopic, {shouldDirty: false, shouldTouch: false});
			setIsTopicInitialized(true);
		},
		[clearSegments, form, guildId, previousValueRef, segmentManagerRef],
	);

	React.useEffect(() => {
		if (!channel) return;
		syncTopicFromMarkdown(channel.topic);
	}, [channel, syncTopicFromMarkdown]);

	React.useEffect(() => {
		if (!isTopicInitialized) return;
		const actualTopic = displayToActual(topicValue);
		const isDirty = actualTopic !== originalTopicRef.current;
		form.setValue('topic', actualTopic, {shouldDirty: isDirty, shouldTouch: false});
		if (!isDirty && form.formState.dirtyFields.topic) {
			const currentValues = form.getValues();
			form.reset({...currentValues, topic: originalTopicRef.current}, {keepValues: true});
		}
	}, [displayToActual, form, isTopicInitialized, topicValue]);

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			if (!channel) return;

			const updateData: Record<string, unknown> = {
				name: data.name,
			};

			if (channel.type === ChannelTypes.GUILD_TEXT) {
				updateData.topic = data.topic;
				updateData.rate_limit_per_user = data.slowmode;
				updateData.nsfw = data.nsfw;
			} else if (channel.type === ChannelTypes.GUILD_VOICE) {
				updateData.bitrate = (data.bitrate ?? 64) * 1000;
				updateData.user_limit = data.user_limit;
				updateData.rtc_region = data.rtc_region ?? null;
			} else if (channel.type === ChannelTypes.GUILD_LINK) {
				updateData.url = data.url;
			}

			await ChannelActionCreators.update(channel.id, updateData);

			const currentValues = form.getValues();
			form.reset({
				name: data.name,
				topic: data.topic ?? '',
				url: data.url ?? '',
				slowmode: data.slowmode ?? currentValues.slowmode ?? 0,
				nsfw: data.nsfw ?? currentValues.nsfw ?? false,
				bitrate: data.bitrate ?? currentValues.bitrate ?? 64,
				user_limit: data.user_limit ?? currentValues.user_limit ?? 0,
				rtc_region: data.rtc_region ?? currentValues.rtc_region ?? null,
			});
			syncTopicFromMarkdown(data.topic ?? '');

			ToastActionCreators.createToast({type: 'success', children: <Trans>Channel updated</Trans>});
		},
		[channel, form, syncTopicFromMarkdown],
	);

	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	const handleReset = React.useCallback(() => {
		if (!channel) return;
		form.reset({
			name: channel.name || '',
			topic: channel.topic || '',
			url: channel.url || '',
			slowmode: channel.rateLimitPerUser || 0,
			nsfw: channel.nsfw || false,
			bitrate: channel.bitrate ? getNearestBitrate(Math.round(channel.bitrate / 1000)) : 64,
			user_limit: channel.userLimit ?? 0,
			rtc_region: channel.rtcRegion ?? null,
		});
		syncTopicFromMarkdown(channel.topic ?? '');
		setTopicExpressionPickerOpen(false);
	}, [channel, form, syncTopicFromMarkdown]);

	const isFormDirty = form.formState.isDirty;
	const hasUnsavedChanges = Boolean(isFormDirty);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(CHANNEL_OVERVIEW_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(CHANNEL_OVERVIEW_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [handleReset, handleSave, form.formState.isSubmitting]);

	React.useEffect(() => {
		return () => {
			UnsavedChangesActionCreators.clearUnsavedChanges(CHANNEL_OVERVIEW_TAB_ID);
		};
	}, []);

	if (!channel) return null;

	const isTextChannel = channel.type === ChannelTypes.GUILD_TEXT;
	const isGuildVoiceChannel = channel.type === ChannelTypes.GUILD_VOICE;
	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
	const isLinkChannel = channel.type === ChannelTypes.GUILD_LINK;

	return (
		<div className={styles.sectionWrapper}>
			<div>
				<h2 className={styles.sectionHeader}>
					<Trans>General</Trans>
				</h2>
				<p className={styles.sectionDescription}>
					{isCategory ? <Trans>Tweak this category's basics.</Trans> : <Trans>Tweak this channel's basics.</Trans>}
				</p>
			</div>

			<Form form={form} onSubmit={handleSave}>
				<Input
					{...form.register('name')}
					type="text"
					label={isCategory ? t`Category Name` : t`Channel Name`}
					placeholder={isCategory ? t`My Category` : t`general`}
					minLength={1}
					maxLength={100}
					error={form.formState.errors.name?.message}
				/>

				{isLinkChannel && (
					<>
						<Input
							{...form.register('url')}
							type="url"
							label={t`URL`}
							placeholder={t`https://example.com`}
							error={form.formState.errors.url?.message}
						/>
						<p className={styles.fieldDescription}>
							<Trans>The URL this link channel points to.</Trans>
						</p>
					</>
				)}

				{isTextChannel && (
					<>
						{topicIsAutocompleteAttached && (
							<Autocomplete
								type={topicAutocompleteType}
								onSelect={topicHandleSelect}
								selectedIndex={topicSelectedIndex}
								options={topicAutocompleteOptions}
								setSelectedIndex={topicSetSelectedIndex}
								referenceElement={topicContainerRef.current}
								query={topicAutocompleteQuery}
								zIndex={SETTINGS_AUTOCOMPLETE_Z_INDEX}
							/>
						)}

						<div ref={topicContainerRef}>
							<Textarea
								ref={topicTextareaRef}
								label={t`Topic`}
								placeholder={t`Add a topic to this channel`}
								maxLength={MAX_TOPIC_LENGTH}
								minRows={3}
								maxRows={6}
								showCharacterCount={true}
								value={topicValue}
								onChange={(event) => {
									const newValue = event.target.value;
									handleTextChange(newValue, previousValueRef.current);
									setTopicValue(newValue);
								}}
								onFocus={() => setIsTopicFieldFocused(true)}
								onBlur={() => setIsTopicFieldFocused(false)}
								onKeyDown={handleTopicKeyDown}
								onKeyUp={topicOnCursorMove}
								onClick={topicOnCursorMove}
								error={form.formState.errors.topic?.message}
								innerActionButton={
									mobileLayout.enabled ? (
										<FocusRing offset={-2}>
											<button
												type="button"
												onClick={() => setTopicExpressionPickerOpen(true)}
												className={clsx(
													styles.emojiButton,
													topicExpressionPickerOpen ? styles.emojiButtonActive : styles.emojiButtonInactive,
												)}
												aria-label={t`Insert emoji`}
											>
												<SmileyIcon size={20} weight="fill" />
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
											returnFocusRef={topicTextareaRef}
											render={({onClose}) => (
												<ExpressionPickerPopout
													channelId={channelId}
													onEmojiSelect={(emoji, shift) => {
														handleTopicEmojiSelect(emoji, shift);
														if (!shift) onClose();
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
													aria-label={t`Insert emoji`}
												>
													<SmileyIcon size={20} weight="fill" />
												</button>
											</FocusRing>
										</Popout>
									)
								}
								characterCountTooltip={(_remaining, total, current) => (
									<CharacterCounter
										currentLength={current}
										maxLength={total}
										isPremium={true}
										premiumMaxLength={total}
										onUpgradeClick={() => undefined}
									/>
								)}
							/>
						</div>
						{mobileLayout.enabled && (
							<ExpressionPickerSheet
								isOpen={topicExpressionPickerOpen}
								onClose={() => setTopicExpressionPickerOpen(false)}
								onEmojiSelect={(emoji, shiftKey) => {
									handleTopicEmojiSelect(emoji, shiftKey);
								}}
								visibleTabs={['emojis']}
								channelId={channelId}
							/>
						)}

						<Controller
							name="slowmode"
							control={form.control}
							render={({field}) => (
								<FormSelect<number>
									label={t`Slowmode`}
									description={t`Members must wait between messages.`}
									value={field.value ?? 0}
									options={slowmodeOptions}
									onChange={(v) => field.onChange(v)}
								/>
							)}
						/>

						<Switch
							label={t`Mark as 18+ (NSFW)`}
							description={t`Members must confirm they are 18 or older to view.`}
							value={form.watch('nsfw') ?? false}
							onChange={(value) => form.setValue('nsfw', value, {shouldDirty: true})}
						/>
					</>
				)}

				{isGuildVoiceChannel && (
					<>
						<div>
							<div className={styles.fieldLabel}>
								<Trans>Voice Quality</Trans>
							</div>
							<Controller
								name="bitrate"
								control={form.control}
								render={({field}) => {
									const sliderValue = typeof field.value === 'number' ? field.value : BITRATE_OPTIONS[1];
									return (
										<div className={styles.fieldContent}>
											<Slider
												defaultValue={sliderValue}
												minValue={BITRATE_OPTIONS[0]}
												maxValue={BITRATE_OPTIONS[BITRATE_OPTIONS.length - 1]}
												markers={Array.from(BITRATE_OPTIONS)}
												stickToMarkers={true}
												onMarkerRender={(value) => <Trans>{value} kbps</Trans>}
												onValueRender={(value) => <Trans>{value} kbps</Trans>}
												factoryDefaultValue={64}
												onValueChange={(value) => field.onChange(value)}
											/>
											<p className={styles.fieldNote}>
												<Trans>Higher bitrate = better quality and higher bandwidth usage.</Trans>
											</p>
										</div>
									);
								}}
							/>
						</div>

						<div>
							<div className={styles.fieldLabel}>
								<Trans>Participant Limit</Trans>
							</div>
							<Controller
								name="user_limit"
								control={form.control}
								render={({field}) => {
									const currentValue = typeof field.value === 'number' ? field.value : 0;
									return (
										<div className={styles.fieldContent}>
											<Slider
												defaultValue={currentValue}
												minValue={0}
												maxValue={99}
												step={1}
												markers={[0, 25, 50, 75, 99]}
												onMarkerRender={(value) => (value === 0 ? '∞' : value)}
												onValueRender={(value) =>
													value === 0 ? <Trans>No limit</Trans> : <Trans>{value} participants</Trans>
												}
												factoryDefaultValue={0}
												onValueChange={(value) => field.onChange(value)}
											/>
											<p className={styles.fieldNote}>
												<Trans>Maximum members who can join at once. 0 means unlimited.</Trans>
											</p>
										</div>
									);
								}}
							/>
						</div>

						{canUpdateRtcRegion && isGuildVoiceChannel && (
							<Controller
								name="rtc_region"
								control={form.control}
								render={({field}) => {
									const RtcRegionOptionWrapper = observer((props: OptionProps<RtcRegionOption>) => {
										const wrappedProps = {...props, getRegionDisplayName} as any;
										return React.createElement(RtcRegionOptionComponent, wrappedProps);
									});
									const RtcRegionSingleValueWrapper = observer((props: SingleValueProps<RtcRegionOption>) => {
										const wrappedProps = {...props, getRegionDisplayName} as any;
										return React.createElement(RtcRegionSingleValue, wrappedProps);
									});

									const options: Array<RtcRegionOption> = [
										{value: null, label: automaticLabel, region: null},
										...rtcRegions.map((region) => ({
											value: region.id,
											label: getRegionDisplayName(region.id, region.name),
											region,
										})),
									];

									return (
										<div className={styles.selectField}>
											<FormSelect
												label={t`Voice Region`}
												description={t`Select a voice region for this channel. Automatic uses the closest region.`}
												value={field.value ?? null}
												onChange={(value) => field.onChange(value)}
												options={options}
												isSearchable={true}
												placeholder={automaticLabel}
												isClearable={false}
												isLoading={isLoadingRegions}
												components={{
													Option: RtcRegionOptionWrapper as any,
													SingleValue: RtcRegionSingleValueWrapper as any,
												}}
												filterOption={(option: any, inputValue: string) => {
													const searchTerm = inputValue.toLowerCase();
													if (!option.data.region) {
														return option.data.label.toLowerCase().includes(searchTerm);
													}
													const displayName = getRegionDisplayName(option.data.region.id, option.data.region.name);
													return (
														displayName.toLowerCase().includes(searchTerm) ||
														option.data.region.id.toLowerCase().includes(searchTerm)
													);
												}}
											/>
										</div>
									);
								}}
							/>
						)}
					</>
				)}
			</Form>
		</div>
	);
});

export default ChannelOverviewTab;
