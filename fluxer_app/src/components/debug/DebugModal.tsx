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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';
import * as Modal from '~/components/modals/Modal';
import {type TabItem, Tabs} from '~/components/uikit/Tabs/Tabs';
import {NodeType} from '~/lib/markdown/parser/types/enums';
import type {CodeBlockNode} from '~/lib/markdown/parser/types/nodes';
import {MarkdownContext} from '~/lib/markdown/renderers';
import {CodeBlockRenderer} from '~/lib/markdown/renderers/common/code-elements';
import markupStyles from '~/styles/Markup.module.css';
import styles from './DebugModal.module.css';

export interface DebugTab {
	id: string;
	label: string;
	data: unknown;
	summary?: React.ReactNode;
	language?: string;
	serialize?: (value: unknown) => string;
}

interface DebugModalProps {
	title: string;
	tabs: Array<DebugTab>;
	defaultTab?: string;
}

export const DebugModal: React.FC<DebugModalProps> = observer(({title, tabs, defaultTab}) => {
	const {i18n} = useLingui();
	const [activeTabId, setActiveTabId] = useState<string>(defaultTab ?? tabs[0]?.id ?? '');

	const activeTab = useMemo(() => tabs.find((tab) => tab.id === activeTabId) ?? tabs[0], [tabs, activeTabId]);
	const tabItems = useMemo<Array<TabItem<string>>>(() => tabs.map(({id, label}) => ({key: id, label})), [tabs]);

	const codeContent = useMemo(() => {
		if (!activeTab) return 'No data available';
		if (activeTab.serialize) {
			try {
				return activeTab.serialize(activeTab.data);
			} catch (error) {
				console.error('Failed to serialize debug tab data via custom serializer:', error);
				return 'Unable to serialize data';
			}
		}

		if (activeTab.data == null) {
			return 'No data available';
		}

		if (typeof activeTab.data === 'string') {
			return activeTab.data;
		}

		try {
			return JSON.stringify(activeTab.data, null, 2);
		} catch (error) {
			console.error('Failed to stringify debug tab data:', error);
			return 'Unable to serialize data';
		}
	}, [activeTab]);

	const codeNode = useMemo<CodeBlockNode>(
		() => ({
			type: NodeType.CodeBlock,
			content: codeContent,
			language: activeTab?.language ?? 'json',
		}),
		[codeContent, activeTab?.language],
	);

	return (
		<Modal.Root size="large">
			<Modal.Header title={title} />
			<Modal.Content padding="none" className={styles.content}>
				<div className={styles.container}>
					{tabs.length > 1 && (
						<div className={styles.tabsSection}>
							<Tabs
								tabs={tabItems}
								activeTab={activeTabId}
								onTabChange={(tabKey) => setActiveTabId(tabKey)}
								className={styles.tabs}
							/>
						</div>
					)}

					<div className={styles.scrollArea}>
						{activeTab?.summary && (
							<section className={styles.summary}>
								<h3 className={styles.summaryTitle}>
									<Trans>Summary</Trans>
								</h3>
								<div className={styles.summaryBody}>{activeTab.summary}</div>
							</section>
						)}

						<div className={styles.codeSection}>
							<div className={clsx(markupStyles.markup, styles.codeSurface)}>
								<CodeBlockRenderer
									id={`${activeTabId}-debug`}
									node={codeNode}
									renderChildren={() => null}
									options={{
										context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
										shouldJumboEmojis: false,
										i18n,
									}}
								/>
							</div>
						</div>
					</div>
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});

interface SummaryItemProps {
	label: string;
	value: React.ReactNode;
}

export const SummaryItem: React.FC<SummaryItemProps> = observer(({label, value}) => (
	<div className={styles.summaryItem}>
		<span className={styles.summaryLabel}>{label}</span>
		<span className={styles.summaryValue}>{value}</span>
	</div>
));
