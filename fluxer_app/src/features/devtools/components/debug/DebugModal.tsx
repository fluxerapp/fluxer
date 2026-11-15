// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/devtools/components/debug/DebugModal.module.css';
import {CodeBlockRenderer} from '@app/features/messaging/components/markdown/renderers/common/CodeElements';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {CodeBlockNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {type TabItem, Tabs} from '@app/features/ui/tabs/Tabs';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

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
}

const logger = new Logger('DebugModal');
export const DebugModal: React.FC<DebugModalProps> = observer(({title, tabs}) => {
	const {i18n} = useLingui();
	const [activeTabId, setActiveTabId] = useState<string>(tabs[0]?.id ?? '');
	const activeTab = useMemo(() => tabs.find((tab) => tab.id === activeTabId) ?? tabs[0], [tabs, activeTabId]);
	const tabItems = useMemo<Array<TabItem<string>>>(() => tabs.map(({id, label}) => ({key: id, label})), [tabs]);
	const codeContent = useMemo(() => {
		if (!activeTab) return 'No data available';
		if (activeTab.serialize) {
			try {
				return activeTab.serialize(activeTab.data);
			} catch (error) {
				logger.error('Failed to serialize debug tab data via custom serializer:', error);
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
			logger.error('Failed to stringify debug tab data:', error);
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
		<Modal.Root size="large" data-flx="devtools.debug.debug-modal.modal-root">
			<Modal.Header title={title} data-flx="devtools.debug.debug-modal.modal-header" />
			<Modal.Content padding="none" className={styles.content} data-flx="devtools.debug.debug-modal.content">
				<div className={styles.container} data-flx="devtools.debug.debug-modal.container">
					{tabs.length > 1 && (
						<div className={styles.tabsSection} data-flx="devtools.debug.debug-modal.tabs-section">
							<Tabs
								tabs={tabItems}
								activeTab={activeTabId}
								onTabChange={(tabKey) => setActiveTabId(tabKey)}
								className={styles.tabs}
								data-flx="devtools.debug.debug-modal.tabs"
							/>
						</div>
					)}
					<div className={styles.scrollArea} data-flx="devtools.debug.debug-modal.scroll-area">
						{activeTab?.summary && (
							<section className={styles.summary} data-flx="devtools.debug.debug-modal.summary">
								<h3 className={styles.summaryTitle} data-flx="devtools.debug.debug-modal.summary-title">
									<Trans comment="Heading in developer debug modals for compact parsed-data metrics.">Summary</Trans>
								</h3>
								<div className={styles.summaryBody} data-flx="devtools.debug.debug-modal.summary-body">
									{activeTab.summary}
								</div>
							</section>
						)}
						<div className={styles.codeSection} data-flx="devtools.debug.debug-modal.code-section">
							<div
								className={clsx(markupStyles.markup, styles.codeSurface)}
								data-flx="devtools.debug.debug-modal.code-surface"
							>
								<CodeBlockRenderer
									id={`${activeTabId}-debug`}
									node={codeNode}
									renderChildren={() => null}
									options={{
										context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
										shouldJumboEmojis: false,
										i18n,
									}}
									data-flx="devtools.debug.debug-modal.code-block-renderer"
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
	<div className={styles.summaryItem} data-flx="devtools.debug.debug-modal.summary-item.summary-item">
		<span className={styles.summaryLabel} data-flx="devtools.debug.debug-modal.summary-item.summary-label">
			{label}
		</span>
		<span className={styles.summaryValue} data-flx="devtools.debug.debug-modal.summary-item.summary-value">
			{value}
		</span>
	</div>
));
