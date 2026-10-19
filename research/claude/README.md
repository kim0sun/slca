# slca
갱신: 2026-10-19 (notes/2026-10-19_shared-readme-draft.md)

## 질문
유지보수 전용. slca 1.4.1(소스 트리; CRAN 반영 여부 저장소 내 미기록), 범주형 지표 SLCM(LTA/LCPA/JLCA).

## 현재 결론
- baseline 18f16f6b: CI·로컬 테스트 통과, open issue 없음. #1(제약 지표 수준 불일치) closed이나 전용 회귀 테스트 없음.

## 미해결
- baseline "Next actions" 3건 미착수 → notes/2026-10-19_shared-readme-draft.md

## 폐기 (이유)
- 없음

## 제안
- #1 회귀 테스트 추가가 가장 싸고 효과 큼. research/ 제외가 .Rbuildignore에 들어갔으니 다음 R CMD check에서 tarball 내용 확인 필요.
- BCH/ML 3단계 SE·CI 시뮬레이션 검증은 유지보수 범위를 넘으니 사용자가 연구로 올릴지 결정.

## 노트 목록
- 2026-10-19 | shared/README.md 초안 | [notes/2026-10-19_shared-readme-draft.md](notes/2026-10-19_shared-readme-draft.md) | 패키지 상태·방침·후보 과제 5건
