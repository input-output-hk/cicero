// Package docs Package classification Cicero API.
//
//     Draft Specification of the Cicero REST API
//
//    *"Am I allowed to deploy this?"* in our system means:
//    *"Does this **artifact** pass all **gates**?* which translates to:
//    *"Do all the artifact's gates have their **KRI**s below their defined threshold?"*
//
//    KRIs are calculated from **certificates** which are a special case of **events**.
//
//    So our basic types are:
//    - Artifact
//    - Gate
//    - KRI
//    - Certificate
//    - Event
//
//    Everything is expressed through these types.
//
//    What exactly an artifact is is up for debate.
//    Here I assume it could be a build result, test result, or deployment action (still a build result but from another perspective, or a build result with deployment info attached).
//
//     Schemes: https
//     BasePath: /
//     Version: 1.0.0
//     Host: https://brain.iohk.io
//
//     Consumes:
//     - application/json
//
//     Produces:
//     - application/json
//
// swagger:meta
package docs