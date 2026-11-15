// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEngineCore} from '@app/features/account_engine/core/AccountEngineCore';
import {SNAPSHOT_SCHEMA_EPOCH} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {CreateAccountEngineCore} from '@fluxer/desktop_ipc/src/AccountEngineCoreContract';

export const createAccountEngineCore: CreateAccountEngineCore = createEngineCore;

export const ACCOUNT_ENGINE_SNAPSHOT_SCHEMA_EPOCH: number = SNAPSHOT_SCHEMA_EPOCH;
