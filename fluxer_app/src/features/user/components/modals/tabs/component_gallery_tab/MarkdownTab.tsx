// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {Message} from '@app/features/channel/components/ChannelMessage';
import {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import {Scroller} from '@app/features/ui/components/Scroller';
import appearanceStyles from '@app/features/user/components/modals/tabs/AppearanceTab.module.css';
import {SubsectionTitle} from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabSubsectionTitle';
import styles from '@app/features/user/components/modals/tabs/component_gallery_tab/MarkdownTab.module.css';
import Users from '@app/features/user/state/Users';
import {MessagePreviewContext, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const TEXT_FORMATTING_DESCRIPTOR = msg({
	message: 'Text formatting',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const HEADINGS_DESCRIPTOR = msg({
	message: 'Headings',
	comment: 'Title in the markdown tab. Keep it concise.',
});
const LINKS_DESCRIPTOR = msg({
	message: 'Links',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const LISTS_DESCRIPTOR = msg({
	message: 'Lists',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const UNORDERED_DESCRIPTOR = msg({
	message: 'Unordered',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const ORDERED_DESCRIPTOR = msg({
	message: 'Ordered',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const NESTED_DESCRIPTOR = msg({
	message: 'Nested',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const BLOCKQUOTES_DESCRIPTOR = msg({
	message: 'Blockquotes',
	comment: 'Button or menu action label in the markdown tab. Keep it concise. Keep the tone plain and specific.',
});
const SINGLE_LINE_DESCRIPTOR = msg({
	message: 'Single line',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const MULTI_LINE_DESCRIPTOR = msg({
	message: 'Multi-line',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const ALTERNATIVE_DESCRIPTOR = msg({
	message: 'Alternative',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const CODE_BLOCKS_DESCRIPTOR = msg({
	message: 'Code blocks',
	comment: 'Short label in the markdown tab. Keep it concise. Keep the tone plain and specific.',
});
const PLAIN_DESCRIPTOR = msg({
	message: 'Plain',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const ALERTS_AND_CALLOUTS_DESCRIPTOR = msg({
	message: 'Alerts and callouts',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const SPECIAL_FEATURES_DESCRIPTOR = msg({
	message: 'Special features',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const SUBTEXT_DESCRIPTOR = msg({
	message: 'Subtext',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const BLOCK_SPOILER_DESCRIPTOR = msg({
	message: 'Block spoiler',
	comment: 'Button or menu action label in the markdown tab. Keep it concise. Keep the tone plain and specific.',
});
const UNICODE_EMOJIS_DESCRIPTOR = msg({
	message: 'Unicode emojis',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const SHORTCODES_DESCRIPTOR = msg({
	message: 'Shortcodes',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const MENTIONS_AND_TIMESTAMPS_DESCRIPTOR = msg({
	message: 'Mentions and timestamps',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const SHORT_TIME_DESCRIPTOR = msg({
	message: 'Short time',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const LONG_TIME_DESCRIPTOR = msg({
	message: 'Long time',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const SHORT_DATE_DESCRIPTOR = msg({
	message: 'Short date',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const LONG_DATE_DESCRIPTOR = msg({
	message: 'Long date',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const DEFAULT_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const FULL_DESCRIPTOR = msg({
	message: 'Full',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const SHORT_DATE_TIME_DESCRIPTOR = msg({
	message: 'Short date/time',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
const RELATIVE_DESCRIPTOR = msg({
	message: 'Relative',
	comment: 'Short label in the markdown tab. Keep it concise.',
});
export const MarkdownTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {fakeChannel, createMessage, markdownSections} = useMemo(() => {
		const currentUser = Users.getCurrentUser();
		const author = currentUser?.toJSON() || {
			id: '1000000000000000010',
			username: 'MarkdownUser',
			discriminator: '0000',
			global_name: 'Markdown Preview User',
			avatar: null,
			avatar_color: null,
			bot: false,
			system: false,
			flags: 0,
		};
		const fakeChannel = new Channel({
			id: '1000000000000000011',
			type: 0,
			name: 'markdown-preview',
			position: 0,
			parent_id: null,
			topic: null,
			url: null,
			nsfw: false,
			last_message_id: null,
			last_pin_timestamp: null,
			bitrate: null,
			user_limit: null,
			permission_overwrites: [],
		});
		const tabOpenedAt = new Date();
		const markdownSections = [
			{
				title: i18n._(TEXT_FORMATTING_DESCRIPTOR),
				items: [
					{label: '**bold**', content: '**bold text**'},
					{label: '*italic*', content: '*italic text*'},
					{label: '***bold italic***', content: '***bold italic***'},
					{label: '__underline__', content: '__underline text__'},
					{label: '~~strikethrough~~', content: '~~strikethrough text~~'},
					{label: '`code`', content: '`inline code`'},
					{label: '||spoiler||', content: '||spoiler text||'},
					{label: '\\*escaped\\*', content: '\\*escaped asterisks\\*'},
				],
			},
			{
				title: i18n._(HEADINGS_DESCRIPTOR),
				items: [
					{label: '#', content: '# Heading 1'},
					{label: '##', content: '## Heading 2'},
					{label: '###', content: '### Heading 3'},
					{label: '####', content: '#### Heading 4'},
				],
			},
			{
				title: i18n._(LINKS_DESCRIPTOR),
				items: [
					{label: '[text](url)', content: '[Masked link](https://fluxer.app)'},
					{label: '<url>', content: '<https://fluxer.app>'},
					{label: 'url', content: 'https://fluxer.app'},
					{label: '<email>', content: '<contact@fluxer.app>'},
				],
			},
			{
				title: i18n._(LISTS_DESCRIPTOR),
				items: [
					{label: i18n._(UNORDERED_DESCRIPTOR), content: '- First item\n- Second item\n- Third item'},
					{label: i18n._(ORDERED_DESCRIPTOR), content: '1. First item\n2. Second item\n3. Third item'},
					{
						label: i18n._(NESTED_DESCRIPTOR),
						content: '- Parent item\n  - Nested item\n  - Another nested item\n- Another parent item',
					},
				],
			},
			{
				title: i18n._(BLOCKQUOTES_DESCRIPTOR),
				items: [
					{label: i18n._(SINGLE_LINE_DESCRIPTOR), content: '> Single-line quote'},
					{
						label: i18n._(MULTI_LINE_DESCRIPTOR),
						content: '> Multi-line quote\n> Spans multiple lines\n> Continues here',
					},
					{
						label: i18n._(ALTERNATIVE_DESCRIPTOR),
						content: '>>> Multi-line quote\nContinues without > on each line\nUntil the message ends',
					},
				],
			},
			{
				title: i18n._(CODE_BLOCKS_DESCRIPTOR),
				items: [
					{label: i18n._(PLAIN_DESCRIPTOR), content: '```\nfunction example() {\n  return "Hello";\n}\n```'},
					{
						label: 'JavaScript',
						content: '```js\nfunction greet(name) {\n  console.log(`Hello, $' + '{name}!`);\n}\n```',
					},
					{
						label: 'Python',
						content: '```py\ndef factorial(n):\n    return 1 if n <= 1 else n * factorial(n-1)\n```',
					},
				],
			},
			{
				title: i18n._(ALERTS_AND_CALLOUTS_DESCRIPTOR),
				items: [
					{label: '[!NOTE]', content: '> [!NOTE]\n> Helpful information here'},
					{label: '[!TIP]', content: '> [!TIP]\n> Useful suggestion here'},
					{label: '[!IMPORTANT]', content: '> [!IMPORTANT]\n> Critical information here'},
					{label: '[!WARNING]', content: '> [!WARNING]\n> Exercise caution here'},
					{label: '[!CAUTION]', content: '> [!CAUTION]\n> Potential risks here'},
				],
			},
			{
				title: i18n._(SPECIAL_FEATURES_DESCRIPTOR),
				items: [
					{label: i18n._(SUBTEXT_DESCRIPTOR), content: '-# This is subtext that appears smaller and dimmed'},
					{label: i18n._(BLOCK_SPOILER_DESCRIPTOR), content: '||\nBlock spoiler content\nClick to reveal!\n||'},
					{label: i18n._(UNICODE_EMOJIS_DESCRIPTOR), content: '🎉 🚀 ❤️ 👍 😀'},
					{label: i18n._(SHORTCODES_DESCRIPTOR), content: ':tm: :copyright: :registered:'},
				],
			},
			{
				title: i18n._(MENTIONS_AND_TIMESTAMPS_DESCRIPTOR),
				items: [
					{label: '@everyone', content: '@everyone'},
					{label: '@here', content: '@here'},
					{label: i18n._(SHORT_TIME_DESCRIPTOR), content: '<t:1618936830:t>'},
					{label: i18n._(LONG_TIME_DESCRIPTOR), content: '<t:1618936830:T>'},
					{label: i18n._(SHORT_DATE_DESCRIPTOR), content: '<t:1618936830:d>'},
					{label: i18n._(LONG_DATE_DESCRIPTOR), content: '<t:1618936830:D>'},
					{label: i18n._(DEFAULT_DESCRIPTOR), content: '<t:1618936830:f>'},
					{label: i18n._(FULL_DESCRIPTOR), content: '<t:1618936830:F>'},
					{label: i18n._(SHORT_DATE_TIME_DESCRIPTOR), content: '<t:1618936830:s>'},
					{label: i18n._(RELATIVE_DESCRIPTOR), content: '<t:1618936830:R>'},
				],
			},
		];
		return {
			fakeChannel,
			createMessage: (content: string, index: string) => {
				return new MessageModel(
					{
						id: `10000000000000001${index.replace('-', '')}`,
						channel_id: '1000000000000000011',
						author,
						type: MessageTypes.DEFAULT,
						flags: 0,
						pinned: false,
						mention_everyone: false,
						content,
						timestamp: tabOpenedAt.toISOString(),
						state: MessageStates.SENT,
					},
					{skipUserCache: true},
				);
			},
			markdownSections,
		};
	}, [i18n.locale]);
	useEffect(() => {
		Channels.handleChannelCreate({channel: fakeChannel.toJSON()});
		return () => {
			Channels.handleChannelDelete({channel: fakeChannel.toJSON()});
		};
	}, [fakeChannel]);
	return (
		<SettingsTabContainer data-flx="user.component-gallery-tab.markdown-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.component-gallery-tab.markdown-tab.settings-tab-content">
				<div className={styles.sectionsContainer} data-flx="user.component-gallery-tab.markdown-tab.sections-container">
					{markdownSections.map((section, sectionIndex) => (
						<div
							key={sectionIndex}
							className={styles.section}
							data-flx="user.component-gallery-tab.markdown-tab.section"
						>
							<div className={styles.sectionHeader} data-flx="user.component-gallery-tab.markdown-tab.section-header">
								<SubsectionTitle data-flx="user.component-gallery-tab.markdown-tab.subsection-title">
									{section.title}
								</SubsectionTitle>
							</div>
							{section.items.map((item, itemIndex) => {
								const message = createMessage(item.content, `${sectionIndex}-${itemIndex}`);
								return (
									<div key={itemIndex} className={styles.item} data-flx="user.component-gallery-tab.markdown-tab.item">
										<div className={styles.itemHeader} data-flx="user.component-gallery-tab.markdown-tab.item-header">
											<code className={styles.itemLabel} data-flx="user.component-gallery-tab.markdown-tab.item-label">
												{item.label}
											</code>
										</div>
										<div
											className={appearanceStyles.previewWrapper}
											data-flx="user.component-gallery-tab.markdown-tab.div"
										>
											<div
												className={clsx(appearanceStyles.previewContainer, appearanceStyles.previewContainerCozy)}
												style={{
													height: 'auto',
													minHeight: '60px',
													maxHeight: '300px',
												}}
												data-flx="user.component-gallery-tab.markdown-tab.div--2"
											>
												<Scroller
													key="markdown-preview-scroller"
													className={appearanceStyles.previewMessagesContainer}
													style={{
														height: 'auto',
														minHeight: '60px',
														maxHeight: '280px',
														pointerEvents: 'auto',
														paddingBottom: '16px',
													}}
													data-flx="user.component-gallery-tab.markdown-tab.scroller"
												>
													<Message
														channel={fakeChannel}
														message={message}
														previewContext={MessagePreviewContext.SETTINGS}
														shouldGroup={false}
														data-flx="user.component-gallery-tab.markdown-tab.message"
													/>
												</Scroller>
												<div
													className={appearanceStyles.previewOverlay}
													data-flx="user.component-gallery-tab.markdown-tab.div--3"
												/>
											</div>
										</div>
									</div>
								);
							})}
						</div>
					))}
				</div>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
