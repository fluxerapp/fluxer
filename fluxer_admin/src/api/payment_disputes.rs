// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::generated::types::{
    AdminPaymentDisputeDecisionRequest, AdminPaymentDisputeDecisionRequestDecision,
    AdminPaymentDisputeDecisionRequestNote, AdminPaymentDisputeReversalRequest,
    AdminPaymentDisputeReversalRequestNote, AdminPaymentDisputeReviewListResponse,
    AdminPaymentDisputeReviewResponse, DecideAdminPaymentDisputeReviewDisputeId,
    ReverseAdminPaymentDisputeReviewDisputeId,
};

impl AdminApiClient {
    pub async fn list_payment_dispute_reviews(
        &self,
    ) -> ApiResult<AdminPaymentDisputeReviewListResponse> {
        let response = self
            .generated()
            .list_admin_payment_dispute_reviews()
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }

    pub async fn decide_payment_dispute_review(
        &self,
        dispute_id: &str,
        decision: AdminPaymentDisputeDecisionRequestDecision,
        note: Option<&str>,
    ) -> ApiResult<AdminPaymentDisputeReviewResponse> {
        let dispute_id = DecideAdminPaymentDisputeReviewDisputeId::try_from(dispute_id)
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let note = note
            .map(AdminPaymentDisputeDecisionRequestNote::try_from)
            .transpose()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = AdminPaymentDisputeDecisionRequest { decision, note };
        let response = self
            .generated()
            .decide_admin_payment_dispute_review(&dispute_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }

    pub async fn reverse_payment_dispute_review(
        &self,
        dispute_id: &str,
        note: &str,
    ) -> ApiResult<AdminPaymentDisputeReviewResponse> {
        let dispute_id = ReverseAdminPaymentDisputeReviewDisputeId::try_from(dispute_id)
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = AdminPaymentDisputeReversalRequest {
            note: AdminPaymentDisputeReversalRequestNote::try_from(note)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .reverse_admin_payment_dispute_review(&dispute_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }
}
